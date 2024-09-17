with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Socket_IO;
--with Ada_Lib.Socket_IO.Client;
with Ada_Lib.Socket_IO.Server;
--with Ada_Lib.Socket_IO; -- .Stream_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
--with GNAT.Sockets;
with Hex_IO;
--with Runtime_Options;
with Camera.Lib;

package body Emulator is

   use type Ada.Streams.Stream_Element;
   use type Ada_Lib.Socket_IO.Index_Type;
   use type Ada.Streams.Stream_Element_Array;
   use type Camera.Value_Type;

   task type Emulator_Task_Type;
   type Emulator_Task_Access is access Emulator_Task_Type;


   Emulator_Task                 : Emulator_Task_Access := Null;
   Pan                           : Camera.Value_Type := 0;
-- Request_Socket                : Ada_Lib.Socket_IO.Stream_IO.Stream_Type;
   Terminate_Request             : constant Ada_Lib.Socket_IO.Buffer_Type (1 .. 6):= (
                                    others => 16#FF# );
   Tilt                          : Camera.Value_Type := 0;

   -------------------------------------------------------------------------
   procedure Convert (
      Command                    : in     Camera.Buffer_Type;
      Start                      : in     Camera.Index_Type;
      Width                      : in     Camera.Index_Type;
      Value                      :    out Camera.Value_Type) is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug, "start" & Start'img & " width" & Width'img);
      Value := 0;
      for Index in Start .. Start + Width - 1 loop
         Value := Value * 16#100# + Camera.Value_Type (Command (Index));
      end loop;
      Log_Out (Debug, "value" & Value'img);
   end Convert;

   -------------------------------------------------------------------------
   procedure Create is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Emulator_Task := new Emulator_Task_Type;
      Log_Out (Debug);
   end Create;

   -------------------------------------------------------------------------
   procedure Get_Request (
      Request_Socket             : in out Ada_Lib.Socket_IO.Server.Accepted_Socket_Type;
      Stop                       :    out Boolean) is
   -------------------------------------------------------------------------

--    type Parameter_Type        is record
--       Value                   : Camera.Value_Type;
--       Index                   : Camera.Index_Type;
--       Width                   : Camera.Index_Type;
--    end record;
--
--    type Parameters_Type       is array (Positive range <>) of Parameter_Type;

      Buffer                     : Ada_Lib.Socket_IO.Buffer_Type (1 .. 100);
      Ack                        : constant Ada_Lib.Socket_IO.Buffer_Type (1 .. 3) := (
                                    16#90#,16#40#,16#FF# );
      Completion                 : constant Ada_Lib.Socket_IO.Buffer_Type (1 .. 6) :=
                                    Ack & ( 16#90#,16#50#,16#FF# );
      End_Mark                   : constant := 16#FF#;
--    Head_Mark                  : constant := 16#90#;
      Index                      : Ada_Lib.Socket_IO.Index_Type :=
                                    Buffer'first;
      Last                       : Ada_Lib.Socket_IO.Index_Type;
--    Mask                       : constant := 16#F0#;
      Command_Head               : constant := 16#81#;
--    Save_Socket_IO_Trace       : Boolean;
--    Save_Stream_Tracing        : Boolean;

      -------------------------------------------------------------------------
      procedure Good_Command (
         Command                 : in     Camera.Buffer_Type) is
      -------------------------------------------------------------------------

--       ----------------------------------------------------------------------
--       procedure Send_Response (
--          Code_1               : in     Camera.Data_Type;
--          Code_2               : in     Camera.Data_Type;
--          Options              : in     Camera.Lib.Options_Type) is
--       ----------------------------------------------------------------------
--
--          Response             : Camera.Maximum_Response_Type;
--          Response_Index       : Camera.Index_Type := 3;
--
--       begin
--          Response (1) := Code_1;
--          Response (2) := Code_2;
--          for Option_Index in Options'range loop
--             declare
--                Option         : Camera.Lib.Option_Type renames Options (Option_Index);
--                Data           : Camera.Value_Type := Option.Value;
--                Shift_Index    : Camera.Index_Type := Response_Index + Option.Width - 1;
--
--             begin
--                for Shift in 1 .. Option.Width loop
--                   Log_Here (Debug, "Response_Index" & Response_Index'img &
--                      " Option_Index" & Option_Index'img &
--                      " Shift_Index" & Shift_Index'img &
--                      " Shift" & Shift'img &
--                      " Data " & Hex_IO.Hex (Integer (Data)));
--                   Response (Shift_Index) := Camera.Data_Type (Data and 16#FF#);
--                   Data := Data / 16#100#;
--                   Response_Index := Response_Index + 1;
--                   Shift_Index := Shift_Index - 1;
--                end loop;
--             end;
--          end loop;
--          Response (Response_Index) := 16#FF#;
--          Log_Here (Debug, "write response");
--          Hex_IO.Dump_8 (Response (Response'first .. Response_Index)'address,
--             Response (Response'first .. Response_Index)'size);
--          Request_Socket.Write (Response (Response'first .. Response_Index));
--       end Send_Response;
--       ----------------------------------------------------------------------

      begin
         Log_in (Debug, "good command first" & Command'first'img);
         delay 0.2;     -- let requester have time to do socket read

hex_io.dump_8 (command'address, command'size, 32);
         case Command (2) is

            when 1 =>
               case Command (3) is
                  when 6 =>
                     case Command (4) is
                        when 2 =>           -- absolute possition
                           Log_Here (Debug, "absolute position");
                           Convert (Command, 7, 4, Pan);
                           Convert (Command, 11, 4, Tilt);

                        when others =>
                           null;

                     end case;

                  when others =>
                     null;

               end case;

            when 9 =>
               case Command (3) is
                  when 6 =>
                     case Command (4) is
                        when 16#12# =>           -- pan tilt position
                           declare
                              Length      : constant := 11;
                              Options     : constant Camera.Options_Type := (
                                 (
                                    Data           => 16#90#,
                                    Start          => 1,
                                    Variable_Width => False
                                 ),(
                                    Data           => 16#50#,
                                    Start          => 2,
                                    Variable_Width => False
                                 ),(
                                    Start          => 3,
                                    Value          => Pan,
                                    Variable_Width => True,
                                    Width          => 4
                                 ),(
                                    Start          => 7,
                                    Value          => Tilt,
                                    Variable_Width => True,
                                    Width          => 4
                                 ),(
                                    Data           => 16#FF#,
                                    Start          => Length,
                                    Variable_Width => False
                                 )
                              );
                              Response    : Camera.Maximum_Response_Type;

                           Begin
                              Log_Here (Debug, "pan til pos inq");
                              Request_Socket.Write (Ack);
--                            Camera.Lib.Base.Apply_Parameters (Response,
--                               Camera.Options_Type'(others => Null_Options));
                              if Debug then
                                 Hex_IO.Dump_8 (Response'address, Length * 8, 32);
                              end if;
                              Request_Socket.Write (Response (Response'first .. Length));
                              return;
                           end;

                        when others =>
                           null;

                     end case;

                  when others =>
                     null;

               end case;

            when others =>
               null;

         end case;
         Request_Socket.Write (Completion);
      end Good_Command;

--    -------------------------------------------------------------------------
--    procedure Pop_Trace is
--    -------------------------------------------------------------------------
--
--    begin
--       if Trace_Socket_IO then
--          Ada_Lib.Socket_IO.Trace := Save_Socket_IO_Trace;
--          Ada_Lib.Socket_IO.Tracing := Save_Stream_Tracing;
--       end if;
--    end Pop_Trace;
--    -------------------------------------------------------------------------
--    procedure Push_Trace is
--    -------------------------------------------------------------------------
--
--    begin
--       if Trace_Socket_IO then
--          Save_Socket_IO_Trace := Ada_Lib.Socket_IO.Trace;
--          Save_Stream_Tracing := Ada_Lib.Socket_IO.Tracing;
--          Ada_Lib.Socket_IO.Trace := False;
--          Ada_Lib.Socket_IO.Tracing := False;
--       end if;
--    end Push_Trace;
--    -------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Stop := False;
      loop
--       Push_Trace;
         Request_Socket.Read (Buffer (Index .. Buffer'last), Last);
--       Pop_Trace;

         Log_Here (Debug and then Last > 0, "last" & Last'img);

         if Last > 0 then
            if Last = 6 and then Buffer (1 .. 6) = Terminate_Request then
               Stop := True;
               Log_Here (Debug, "stopping");
               exit;
            end if;

            if Buffer (Buffer'first) = Command_Head and then Buffer (Last) =
                  End_Mark then
               Good_Command (Buffer (Buffer'first .. Last));
               exit;
            else
               for I in Buffer'first .. Last loop
                  if Buffer (I) = End_Mark then
                     Put_Line ("end mark in middle of buffer");
                     exit;
                  end if;
               end loop;
               Index := Last + 1;
               Log_Here (Debug, "index", Index'img);
            end if;
         else
            delay 0.1;
         end if;
      end loop;

      Log_Out (Debug, "last" & Last'img & " doing request");

      if Debug then
         Hex_IO.Dump_8 (Buffer'address, Buffer (Buffer'first .. Last)'size, 32);
      end if;
      delay 0.1;
   end Get_Request;

   -------------------------------------------------------------------------
   procedure Halt (
      Camera                     : in out Standard.Camera.Lib.Base.
                                             Base_Camera_Type'class) is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Camera.Write (Terminate_Request);
      delay 1.0;           -- let task terminate
      Log_Out (Debug);
   end Halt;

   -------------------------------------------------------------------------
   function Has_Emulator return Boolean is
   -------------------------------------------------------------------------

   begin
      return Emulator_Task /= Null;
   end Has_Emulator;

   -------------------------------------------------------------------------
   task body Emulator_Task_Type is

      Server_Socket              : Ada_Lib.Socket_IO.Server.Server_Socket_Type (Port);
      Request_Socket             : Ada_Lib.Socket_IO.Server.Accepted_Socket_Type;
      Stop                       : Boolean := False;

   begin
      Log_In (Debug, "port" & Port'img);
      Ada_Lib.Trace_Tasks.Start;
      begin
         Log_Here (Debug, "wait for accept");
         Server_Socket.Accept_Socket (Request_Socket, 0.2, 0.0);
--       Server_Socket.Set_Option (Ada_Lib.Socket_IO.Reuse_Address);
--       Request_Socket.Set_Option (Ada_Lib.Socket_IO.Reuse_Address);

         Log_Here (Debug, "accept request");
--       Request_Socket.Create (Ada_Lib.Socket_IO.Socket_Type (Request_Socket), 0.2, 0.2);

         Log_Here (Debug, "enter read loop");
         while not Stop loop
            Get_Request (Request_Socket, Stop);
         end loop;

      exception
         when Fault: others =>
            Trace_Exception (Fault);
      end;

      Log_Here (Debug, "stoping");
      Request_Socket.Close;
      Request_Socket.Close;
      Server_Socket.Close;
      Ada_Lib.Trace_Tasks.Stop;
      Emulator_Task := Null;     -- don't bother freeing it
      Log_Out (Debug);
   end Emulator_Task_Type;
   -------------------------------------------------------------------------
end Emulator;
