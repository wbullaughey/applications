-- force update to github 7/14/25 --
--
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Commands.PTZ_Optics;
with Hex_IO;
with Video.Lib;

package body Camera.Lib.Base is

   use type Ada_Lib.Time.Time_Type;
   use type Data_Type;
   use type Index_Type;
   use type Value_Type;

   ----------------------------------------------------------------------------
   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Options                    : in     Options_Type) is
   ----------------------------------------------------------------------------

   begin
      for Index in Options'range loop
         declare
            Option               : Option_Type renames Options (Index);

         begin
            Log_Here (Debug, "Index " & Index'img & " Start " & Option.Start'img);

            case Option.Mode is

               when Add =>
                  Buffer (Option.Start) := Buffer (Option.Start) + Option.Data;
                  Log_Here (Debug, "buffer " &
                     Video.Lib.Hex (Buffer (Option.Start)));

               when Fixed =>
                  Log_Here (Debug, "data " & Video.Lib.Hex (Option.Data));
                  Buffer (Option.Start) := Option.Data;

               when Variable =>
                  declare
                     Buffer_Index: Index_Type := Option.Start + Option.Width - 1;
                     Value       : Value_Type := Option.Value;

                  begin
                     Log_Here (Debug, "Buffer_Index" & Buffer_Index'img &
                        " width" & Option.Width'img);
                     for Counter in 1 .. Option.Width loop
                        Buffer (Buffer_Index) := Data_Type (Value and 16#0F#);
                        Buffer_Index := Buffer_Index - 1;
                        Value := Value / 16#10#;
                     end loop;
                  end;
            end case;
         end;
      end loop;

      if Debug then
         Hex_IO.Dump_8 (Buffer'address, Buffer'size, 32, "from " & Here);
      end if;

   end Apply_Parameters;

   ----------------------------------------------------------------------------
   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Command                    : in     Buffer_Type;
      Options                    : in     Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Buffer (Buffer'first .. Command'last) := Command;
      Apply_Parameters (Buffer, Options);
      Log_Out (Debug);
   end Apply_Parameters;

   ---------------------------------------------------------------
   procedure Dump_Input_Buffer (
      Camera                     : in out Base_Camera_Type;
      From                       : in     String := Here) is
   ---------------------------------------------------------------

   begin
      Camera.Socket.Dump_Input_Buffer (From);
   end Dump_Input_Buffer;

   ---------------------------------------------------------------
   procedure Camera_No_Found (
      Address                    : in     String;
      Port                       : in     GNAT.Sockets.Port_Type) is
   ---------------------------------------------------------------

   begin
      raise Failed with Quote ("Camera address", Address) &
         " port" & Port'img & " not found";
   end Camera_No_Found;

   ---------------------------------------------------------------
   overriding
   procedure Close (
      Camera                     : in out Base_Camera_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Camera.Socket.Close;
--    Camera.Socket.Close;
      Log_Out (Debug);
   end Close;

   ---------------------------------------------------------------
   procedure Get_Response (
      Camera                     : in out Base_Camera_Type;
      Expect_Ack                 : in     Boolean;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration) is
   ---------------------------------------------------------------

      ------------------------------------------------------------
      procedure Failure (
         Message                 : in     String;
         From                    : in     String := Here) is
      ------------------------------------------------------------

         Text                    : constant String := Message &
                                    " from " & From;
      begin
         Log_Exception (Debug, Text);
         raise Failed with Text;
      end Failure;

      ------------------------------------------------------------

      Ack_Length                 : constant Index_Type :=
                                    Base_Camera_Type'class (Camera).Get_Ack_Length;
      End_Read                   : Index_Type;
--    Got_Ack                    : Boolean := False;
      Read_Length                : Index_Type := Ack_Length;   -- min read length
      Start_Read                 : Index_Type := Response'first;
      Timeout                    : Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + Response_Timeout;
   begin
      Log_In (Debug, "Expect_Ack " & Expect_Ack'img &
         " ack length" & Ack_Length'img &
         " Expect_Response " & Expect_Response'img &
         " Response_Length" & Response_Length'img &
         " response first" & Response'first'img &
         " Response_Timeout " & Response_Timeout'img &
         " timeout " & Ada_Lib.Time.Image (Timeout));

      loop  -- look for multiple acks
         declare
            Time_Left      : constant Duration := Timeout - Ada_Lib.Time.Now;
            Response_Code  : constant Data_Type := Response (Start_Read);

         begin
            End_Read := Start_Read + Read_Length - 1;

            Log_Here (Debug, "Start_Read" & Start_Read'img &
               " end read" & End_Read'img &
               " Read_Length" & Read_Length'img &
               " time left " & Time_Left'img);

            Camera.Socket.Read (Response (Start_Read .. End_Read), Time_Left);
            if Debug then
                  Video.Lib.Dump ("response", Response (Start_Read .. End_Read),
                     Natural (Ack_Length));
               end if;

            case Response_Code and 16#F0# is

               when 16#90# =>    -- ack, completion or error
                  declare
                     Response_Subcode  : constant Data_Type :=
                                       Response (Start_Read + 1);
                  begin
                     case Response_Subcode is

                        when 16#40# | 16#41# | 16#42# => -- Ack
                           if Response (End_Read) = 16#FF# then -- end of Ack
                              Log_Here (Debug, "got ack");
                              if not Expect_Response then
                                 exit;
                              end if;
                              -- is the response already in buffer
                              declare
                                 Buffer_Count   : constant Index_Type :=
                                                   Camera.Socket.In_Buffer;
                              begin
                                 Log_Here (Debug, "buffer count" &
                                    Buffer_Count'img);

                                 if Buffer_Count = 0 then
                                    Start_Read := Response'first;
                                 else
                                          Start_Read := Start_Read + Ack_Length;
                                 end if;
                              end;
                              Read_Length := Ack_Length;   -- min read length
                           -- else not ack, set up to read remainder
                           end if;

                     when 16#50# | 16#51# | 16#52#  => -- Completion
                        if not Expect_Response then
                           Failure ("unexpected response");
                        end if;
                        if Response (End_Read) = 16#FF# then -- end of completion
                           Log_Here (Debug, "got short completion. " &
                              " expect response " & Expect_Response'img);
                           -- could be from previous command
                           if not Expect_Response then
                              exit;
                           end if;
                        else
                           -- read the rest of the completion
                           Start_Read := Start_Read + Ack_Length;
                           Read_Length := Response_Length - Ack_Length;


                              if Read_Length > 0 then
                                 End_Read := Start_Read + Read_Length - 1;
                                 Camera.Socket.Read (Response (Start_Read .. End_Read),
                                    Time_Left);
                                 if Debug then
                                    Video.Lib.Dump ("response",
                                       Response, Natural (Response_Length));
                                 end if;
                              end if;
                           end if;
                           exit;

                        when 16#60# .. 16#6F# => -- Error
                           declare
                              Error_Code  : constant Data_Type :=
                                             Response (3);
                           begin
                              --
                              Start_Read := Start_Read + Ack_Length;
                              Read_Length := 1;
                              End_Read := Start_Read + Read_Length - 1;
                              Camera.Socket.Read (Response (Start_Read .. End_Read),
                                 Time_Left);
                              if Debug then
                                 Video.Lib.Dump ("response", Response (Response'first ..
                                    End_Read), Natural (Ack_Length + 1));
                              end if;
                              Log_Here (Debug, "error code" & Response (3)'img);
                              case Error_Code is

                                 when 2 =>      -- bad format
                                    Put_Line ("camera bad format error");

                                 when 3 =>
                                    Put_Line ("multile socet command");

                                 when 4 =>   -- command canceled
                                    Put_Line ("type 4 command canceled");

                                 when 5 =>
                                    Put_Line ("type 5 command canceled");

                                 when 16#41# =>
                                    Put_Line ("type 41 command cannot be executed");

                                 when others =>
                                    Failure ("unexpected error code " &
                                       Ada_Lib.Socket_IO.Hex (Error_Code));

                              end case;
                              exit;
                           end;

                        when others =>    -- unexpected
                           Failure ("unexpected resonse" &
                              Ada_Lib.Socket_IO.Hex (Response_Subcode));

                     end case;
                  end;

               when others =>          -- unexpected
                  Put_Line ("" &
                     Response_Code'img);
                  Failure ("unexpected command header " &
                     Ada_Lib.Socket_IO.Hex (Response_Code));
            end case;
         end;

         -- reset timout time
         Timeout := Ada_Lib.Time.Now + Response_Timeout;
      end loop;
      Log_Out (Debug);

   exception
      when Fault : others =>
         Log_Exception (Debug, Fault, "timeout in Get_Response Read");
         raise;

   end Get_Response;

   ---------------------------------------------------------------
   overriding
   procedure Host_Open (
      Camera                     :    out Base_Camera_Type;
      Host_Address               : in     String;
      Port                       : in     GNAT.Sockets.Port_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Host_Address " & Host_Address & " port" & Port'img);
      Camera.Socket.Connect (Host_Address, Port);
      Log_Out (Debug);

   exception

      when GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Host_Address, Port);

   end Host_Open;

   ---------------------------------------------------------------
   overriding
   procedure IP_Open (
      Camera                     :    out Base_Camera_Type;
      IP_Address                 : in     Ada_Lib.Socket_IO.IP_Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type) is
   ---------------------------------------------------------------

      Address                    : constant String :=
                                    Ada_Lib.Socket_IO.Image (IP_Address);
   begin
--Log_Here (Debug'img & " IP Address" & Address & " port" & Port'img);
      Log_In (Debug, "IP Address" & Address & " port" & Port'img);
--    Camera.Socket.Set_Option (Ada_Lib.Socket_IO.Reuse_Address);
      Camera.Socket.Connect (Address, Port);
--    Camera.Socket.Create (Camera.Socket, 1.0, 0.5);
      Log_Out (Debug);

   exception

      when GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Address, Port);

   end IP_Open;

   ---------------------------------------------------------------
   overriding
   procedure Open (
      Camera                     :    out Base_Camera_Type;
      Address                    : in     Ada_Lib.Socket_IO.Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Address " & Address.Image & " port" & Port'img);
      Camera.Socket.Connect (Address, Port,
         Default_Read_Timeout    => Standard.Camera.Commands.PTZ_Optics.Default_Read_Timeout,
         Default_Write_Timeout   => Standard.Camera.Commands.PTZ_Optics.Default_Write_Timeout);
      Log_Out (Debug);

   exception

      when GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Address.Image, Port);

   end Open;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Timeout_Time               : in     Duration := 0.0) is
                                          -- when 0 use command default
   ---------------------------------------------------------------

      Response                   : Maximum_Response_Type;

   begin
      Log_In (Debug, "command " & Command'img);
      Base_Camera_Type'class (Camera).Process_Command (Command, Options,
         Response, Timeout_Time); -- default with no value returned
      Log_Out (Debug);
   end Process_Command;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0) is
                                          -- when 0 use command default
   ---------------------------------------------------------------

      Get_Ack           : Boolean;
      Response_Length   : Index_Type;
      Has_Response      : Boolean;
      Timeout           : constant Duration := (if Timeout_Time = 0.0 then
                              Base_Camera_Type'class (Camera).Get_Timeout (
                                 Command)
                           else
                              Timeout_Time);
   begin
      Log_In (Debug or List_Commands, "command " & Command'img &
         " Timeout_Time " & Timeout_Time'img &
         " timeout " & Timeout'img);
      Base_Camera_Type'class (Camera).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      Log_Here (Debug, "get ack " & Get_Ack'img &
         " return package " & Has_Response'img &
         " response length" & Response_Length'img);

      Camera.Get_Response (Get_Ack, Has_Response, Response,
         Response_Length, Timeout);  -- get response

      if Debug and then Has_Response then
         Dump ("package response", Response (
            Response'first .. Response_Length));
      end if;

      Log_Out (Debug or List_Commands);

   exception
      when Fault : others =>
         Log_Exception (Debug, Fault, "in Process_Command");
         raise;

   end Process_Command;

   ---------------------------------------------------------------
   overriding
   procedure URL_Open (
      Camera                     :    out Base_Camera_Type;
      URL                        : in     String;
      Port                       : in     GNAT.Sockets.Port_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("URL", URL) & " port" & Port'img);
      Camera.Socket.Connect (URL, Port);
--    Camera.Socket.Create (Camera.Socket, 1.0, 0.5);
      Log_Out (Debug);

   exception

      when Fault: GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         declare
            Message              : constant String :=
                                       Quote ("URL", URL) & " port" & Port'img &
                                          " not found";
         begin
            Trace_Message_Exception (Debug, Fault, Message);
            raise Failed with Message;
         end;

   end URL_Open;

   ---------------------------------------------------------------
   procedure Write (
      Camera                     :    out Base_Camera_Type;
      Data                       : in     Data_Type) is
   ---------------------------------------------------------------

      Buffer                     : constant Buffer_Type (1 .. 1) := (
                                    1 => Data
                                 );
   begin
      Log_In (Debug, "write" & Data'img);
      Camera.Socket.Write (Buffer);
      Log_Out (Debug);
   end Write;

   ---------------------------------------------------------------
   procedure Write (
      Camera                     :    out Base_Camera_Type;
      Data                       : in     Buffer_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "write");
      if Debug then
         Dump ("camera command", Data);
      end if;
      Camera.Socket.Write (Data);
      Log_Out (Debug);
   end Write;

begin
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end Camera.Lib.Base;
