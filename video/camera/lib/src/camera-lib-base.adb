with Ada.Exceptions;
--with ada.text_io;
with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Lib.Connection;
with Camera.Command_Queue;
with Hex_IO;
with Video.Lib;

package body Camera.Lib.Base is

   use type Ada_Lib.Time.Time_Type;
   use type Data_Type;
-- use type Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access;
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
            Log_Here (Debug, "Index " & Index'img & " Start " & Option.Start'img &
               " Variable_Width " & Option.Variable_Width'img);

            case Option.Variable_Width is

               when False =>
                  Log_Here (Debug, "data " & Video.Lib.Hex (Option.Data));
                  Buffer (Option.Start) := Option.Data;

               when True =>
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
         Hex_IO.Dump_8 (Buffer'address, Buffer'size, 32);
      end if;

   end Apply_Parameters;

   ----------------------------------------------------------------------------
   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Command                    : in     Response_Type;
      Options                    : in     Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Buffer (Buffer'first .. Command'last) := Command;
      Apply_Parameters (Buffer, Options);
      Log_Out (Debug);
   end Apply_Parameters;

   ---------------------------------------------------------------
   procedure Camera_No_Found (
      Address                    : in     String;
      Port                       : in     GNAT.Sockets.Port_Type;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "exception name " & Ada.Exceptions.Exception_Name (Fault) &
         " message " & Ada.Exceptions.Exception_Message (Fault));

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

      Ack_Length                 : constant Index_Type :=
                                    Base_Camera_Type'class (Camera).Get_Ack_Length;
      Got_Ack                    : Boolean := False;
      Length                     : Index_Type := Response_Length;
      Timeout                    : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + Response_Timeout;
   begin
      Log_In (Debug, "Expect_Ack " & Expect_Ack'img &
         " ack length" & Ack_Length'img &
         " Expect_Response " & Expect_Response'img &
         " Response_Length" & Response_Length'img &
         " Response_Timeout " & Response_Timeout'img &
         " timeout " & Ada_Lib.Time.Image (Timeout));
      Camera.Socket.Read (Response (Response'first .. Ack_Length), Response_Timeout);
      if Debug then
         Video.Lib.Dump ("response", Response (Response'first .. Ack_Length),
            Natural (Ack_Length));
      end if;

      if Response (Ack_Length) = 16#FF# then -- end of Ack
         Log_Here (Debug, "got ack");
         Got_Ack := True;
      elsif Expect_Ack then
         Log_Exception (Debug, "missing Ack");
         raise Failed with "missing Ack";
      else                       -- did not get an ack
         Length := Length - Ack_Length;
      end if;

      if Expect_Response then
         declare
            End_Read          : constant Index_Type := Response_Length +
                                 (if Got_Ack then
                                    Ack_Length
                                 else
                                    0);
         begin
            Log_Here (Debug, " End_Read" & End_Read'img);

            Camera.Socket.Read (Response (Ack_Length + 1 .. End_Read),
               Response_Timeout);
            if Debug then
               Video.Lib.Dump ("response", Response ((if Got_Ack then
                     Ack_Length + 1
                  else
                     Response'first)
                   .. End_Read),
                  Natural (Response_Length));
            end if;

            if Response (End_Read) /= 16#FF# then
               Log_Exception (Debug, "missing FF at end of response");
               raise FAiled with "missing FF at end of response";
            end if;

            if Got_Ack then     -- move response up to head
               declare
                  Put         : Index_Type := Response'first;
               begin
                  for Index in Ack_Length + 1 .. End_Read loop
                     Response (Put) := Response (Index);
                     Put := Put + 1;
                  end loop;
               end;
            end if;
            if Debug then
               Video.Lib.Dump ("response", Response, Natural (Response_Length));
            end if;
         end;
      end if;
      Log_Out (Debug);
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

      when Fault: GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Host_Address, Port, Fault);

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

      when Fault: GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Address, Port, Fault);

   end IP_Open;

   ---------------------------------------------------------------
   overriding
   procedure Open (
      Camera                     :    out Base_Camera_Type;
      Address                    : in     Ada_Lib.Socket_IO.Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Address " & Address.Image & " port" & Port'img &
         " tag " & Tag_Name (Base_Camera_Type'class (Camera)'tag));
      Camera.Socket.Connect (Address, Port);
      Log_Out (Debug);

   exception

      when Fault: GNAT.Sockets.Host_Error | Ada_Lib.Socket_IO.Failed =>
         Camera_No_Found (Address.Image, Port, Fault);

   end Open;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type) is
   ---------------------------------------------------------------

      Response                   : Maximum_Response_Type;
      Response_Length            : Index_Type;

   begin
      Log_In (Debug, "command " & Command'img);
      Base_Camera_Type'class (Camera).Process_Command (Command, Options,
         Response, Response_Length); -- default with no value returned
      Log_Out (Debug);
   end Process_Command;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type) is
   ---------------------------------------------------------------

      Get_Ack                    : Boolean;
      Has_Response               : Boolean;
      Timeout                    : constant Duration :=
                                    Base_Camera_Type'class (Camera).
                                       Get_Timeout (Command);
   begin
      Log_In (Debug, "command " & Command'img &
         " testing " & Ada_Lib.Unit_Testing'img &
         " queue failed " & Standard.Camera.Command_Queue.Has_Queue_Failed'img &
         " timeout " & Timeout'img);
      if    Ada_Lib.Unit_Testing and then
            Standard.Camera.Command_Queue.Has_Queue_Failed then
         raise Failed with "command queue failed";
      end if;

      if Camera.Waiting_For_Response then
         raise Failed with "outstanding request " & Camera.Last_Command'img;
      end if;

      Base_Camera_Type'class (Camera).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      Log_Here (Debug, "get ack " & Get_Ack'img &
         " return package " & Has_Response'img &
         " response length" & Response_Length'img);

      if Get_Ack or else Has_Response then
         Camera.Waiting_For_Response := True;
         Camera.Last_Command := Command;
      end if;

      Camera.Get_Response (Get_Ack, Has_Response, Response,
         Response_Length, Timeout);  -- get response

      if Debug and then Has_Response then
         Dump ("package response", Response (
            Response'first .. Response_Length));
      end if;

      Camera.Waiting_For_Response := False;
      Camera.Last_Command := No_Command;

      Log_Out (Debug);

   exception
      when Fault : others =>
         Trace_Exception (Fault, Here);
         raise;

   end Process_Command;

   ---------------------------------------------------------------
   function Synchronous (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) return Status_Type is
   ---------------------------------------------------------------

   begin
      return Camera.Queue (Command, Options);
   end Synchronous;

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
   Log_Here (Debug or Elaborate or Trace_Options);
end Camera.Lib.Base;
