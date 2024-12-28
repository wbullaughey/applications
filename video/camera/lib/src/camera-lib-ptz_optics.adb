with Ada.Calendar;
with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Time;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Base;
with Hex_IO;
--with Ada_Lib.Socket_IO.Stream_IO;

package body Camera.Lib.PTZ_Optics is

   use type Ada.Calendar.Time;
   use type Ada.Streams.Stream_Element;
   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;
   use type Preset_ID_Type;
   use type Status_Type;
   use type Video.Lib.Index_Type;

   Default_Response_Timeout      : constant Duration := 0.5;
   Position_Timeout              : constant Duration := 1.0;

   Commands                      : constant Array (Standard.Camera.
                                    Commands_Type) of Standard.Camera.
                                       Lib.Base.Command_Type := (
      Standard.Camera.Auto_Focus           => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#02#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Manual_Focus         => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#03#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Absolute    => ( 15, ( 16#81#,16#01#,16#06#,16#02#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#FF#, others => 0), Required, Position_Timeout, False, 0),
      Standard.Camera.Position_Down_Left   => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#02#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Down_Right  => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#02#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Down        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#02#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Left        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#03#,16#FF#, others => 0 ), None, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Relative    => ( 15, ( 16#81#,16#01#,16#06#,16#03#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#FF#, others => 0 ), Required, Position_Timeout, False, 0),
      Standard.Camera.Position_Request     => ( 5, ( 16#81#,16#09#,16#06#,16#12#,16#FF#, others => 0 ), Optional, Default_Response_Timeout, True, 11),
      Standard.Camera.Position_Right       => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#03#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Stop        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#03#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Up          => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#01#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Up_Left     => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#01#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Position_Up_Right    => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#01#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Memory_Recall        => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#00#,16#FF#, others => 0 ), Required, Position_Timeout, False, 0),
      Standard.Camera.Memory_Set           => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#01#,16#00#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Memory_Reset         => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#00#,16#00#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Power_Set            => ( 6, ( 16#81#,16#01#,16#04#,16#00#,16#00#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Power_Request        => ( 5, ( 16#81#,16#09#,16#04#,6#00#,16#FF#, others => 0 ), Optional, Default_Response_Timeout, True, 4),
      Standard.Camera.Zoom_Direct          => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#00#,16#FF#, others => 0 ), Required, Default_Response_Timeout, False, 0),
      Standard.Camera.Zoom_Full            => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#00#,16#FF#, others => 0 ), Required, Position_Timeout, False, 0),
      Standard.Camera.Zoom_Inquire         => ( 5, ( 16#81#,16#09#,16#04#,16#47#,16#FF#, others => 0 ), None, Position_Timeout, True, 7),
      Standard.Camera.No_Command           => ( 0, ( others => 0 ), None, 0.0, false, 0)
   );

   ----------------------------------------------------------------------------
   overriding
   procedure Acked (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Response_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type) is
   ----------------------------------------------------------------------------

   begin
      Value := Natural (Response (2));
      Log_In (Debug, "value " & Hex_IO.Hex (Value, 8));
      if    Response (1) = 16#90# and then
            ((Response (2) and 16#F0#) = 16#40#) and then
            Response (3) = 16#FF# then
         Next_Buffer_Index := 4;
         Log_Out (Debug, "next buffer " & Next_Buffer_Index'img);
      else
         Next_Buffer_Index := 0;
         Log_Exception (Debug, "invalid ack");
         if Debug then
            Hex_IO.Dump_8 (Response'address, Response'size, 32);
         end if;
         raise Failed with "invalid ack";
      end if;
   end Acked;

   ----------------------------------------------------------------------------
   overriding
   procedure Completed (
      Camera                     : in     PTZ_Optics_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type) is
   pragma Unreferenced (Camera, Buffer, Start, Completion_Value, Next_Byte);
   ----------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Completed;

   ----------------------------------------------------------------------------
   overriding
   procedure Get_Absolute (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type) is
   ---------------------------------------------------------------------------

      Accumulator                : Interfaces.Unsigned_16;
      Conversion                 : Absolute_Type;
      for Conversion'address use Accumulator'address;
      Response_Buffer            : Maximum_Response_Type;
      Response_Length            : Index_Type;

   begin
      Log_In (Debug);
      Camera.Process_Command (Position_Request,
         Options              => Null_Options,
         Response             => Response_Buffer,
         Response_Length      => Response_Length,
         Wait_Until_Finished  => True);

--    if Debug then
--       Response.Dump;
--    end if;

      Accumulator := 0;
      for I in Index_Type'(3) .. 6 loop
         Log_Here (Debug, I'img & ": " &
            Ada_lib.Socket_IO.Hex (Response_Buffer (I)));
         Accumulator := Accumulator * 16#10# +
            Interfaces.Unsigned_16 (Response_Buffer (I) and 16#F#);
      end loop;
      Log_Here (Debug, Hex_IO.Hex (Accumulator) &
         " conversion" & Conversion'img);
      Pan := Conversion;

      Accumulator := 0;
      for I in Index_Type'(7) .. 10 loop
         Log_Here (Debug, I'img & ": " &
            Ada_lib.Socket_IO.Hex (Response_Buffer (I)));
         Accumulator := Accumulator * 16#10# +
            Interfaces.Unsigned_16 (Response_Buffer (I) and 16#F#);
      end loop;
      Log_Here (Debug, Hex_IO.Hex (Accumulator) &
         " conversion" & Conversion'img);
      Tilt := Conversion;

   exception
      when Fault : others =>
         Log_Exception (Debug, Fault);
         raise;

   end Get_Absolute;

   ----------------------------------------------------------------------------
   overriding
   function Get_Ack_Length (
      Camera                     : in     PTZ_Optics_Type
   ) return Index_Type is
   ----------------------------------------------------------------------------

   begin
      return 3;
   end Get_Ack_Length;

   ----------------------------------------------------------------------------
   overriding
   function Get_Default_Preset (
      Camera                     : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ----------------------------------------------------------------------------

   begin
      return Powerup_Preset;
   end Get_Default_Preset;

   ----------------------------------------------------------------------------
   overriding
   procedure Get_Power (
      Camera                     : in out PTZ_Optics_Type;
      Power                      :    out Boolean) is
   ---------------------------------------------------------------------------

      Response_Buffer            : Maximum_Response_Type;
      Response_Length            : Index_Type;

   begin
      Log_In (Debug);
      Camera.Process_Command (Power_Request,
         Options              => Null_Options,
         Response             => Response_Buffer,
         Response_Length      => Response_Length,
         Wait_Until_Finished  => False);

      case Response_Buffer (3) is

         when 2 =>
            Power := True;

         when 3 =>
            Power := False;

         when 4 =>
            raise Failed with "camera power failure code" &
               Response_Buffer (Response_Buffer'first)'img;

         when others =>
            raise Failed with "unexpected power value" &
               Response_Buffer (Response_Buffer'first)'img;

      end case;
      Log_Out (Debug, "power " & Power'img);
   end Get_Power;

   ----------------------------------------------------------------------------
   overriding
   function Get_Timeout (
      Camera                     : in     PTZ_Optics_Type;
      Command                    : in     Commands_Type
   ) return Duration is
   ----------------------------------------------------------------------------

   begin
      return Commands (Command).Response_Timeout;
   end Get_Timeout;

   ---------------------------------------------------------------
   overriding
   procedure Get_Zoom (
      Camera                     : in out PTZ_Optics_Type;
      Zoom                       :    out Absolute_Type) is
   ---------------------------------------------------------------

      Last_Zoom                   : Absolute_Type := Absolute_Type'last;

   begin
      Log_In (Debug);
      loop
         declare
            Accumulator          : Interfaces.Unsigned_16;
            Conversion           : Absolute_Type;
            for Conversion'address use Accumulator'address;
            Response             : Response_Buffer_Type;
            Timeout              : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + 60.0;
            Status               : constant Status_Type := Camera.Synchronous (
                                    Command              => Zoom_Inquire,
                                    Options              => Null_Options,
                                    Wait_Until_Finished  => False);
         begin
            if Status /= Success then
               raise Failed with "Synchronous failed with " & Status'img;
            end if;

            if Debug then
               Response.Dump ("zoom");
            end if;

            Accumulator := 0;
            for I in Index_Type'(3) .. 6 loop
               Log_Here (Debug, I'img & ": " &
                  Ada_lib.Socket_IO.Hex (Response.Buffer (I)));
               Accumulator := Accumulator * 16#10# +
                  Interfaces.Unsigned_16 (Response.Buffer (I) and 16#F#);
            end loop;
            Log_Here (Debug, Hex_IO.Hex (Accumulator) &
               " conversion" & Conversion'img);
            Zoom := Conversion;

            if Last_Zoom /= Absolute_Type'last then
               declare
                  Delta_Zoom            : constant Integer := abs (
                                          Integer (Last_Zoom) - Integer (Zoom));
                  Delta_Message        : constant String :=
                                          " delta zoom" & Delta_Zoom'img;
               begin
                  Log_Here (Debug, "Zoom" & Zoom'img & Delta_Message);
                  if    Delta_Zoom < 2 then
                     Log_Out (Debug);
                     return;
                  elsif Ada_Lib.Time.Now > Timeout then
                     declare
                        Message        : constant String :=
                                          "Get_Absolute did not converge. " &
                                          Delta_Message;
                     begin
                        Log_Exception (Debug, Message);
                        raise Failed with Message;
                     end;
                  end if;
               end;
            end if;

            Last_Zoom := Zoom;
         end;
      end loop;
   end Get_Zoom;

   ---------------------------------------------------------------
   overriding
   function Last_Preset (
      Camera_Queue               : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ---------------------------------------------------------------

   begin
      return Max_Preset;
   end Last_Preset;

   ---------------------------------------------------------------
   overriding
   function Minimum_Test_Preset (
      Camera_Queue               : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ---------------------------------------------------------------

   begin
      return Minimum_Preset_ID_For_Testing;
   end Minimum_Test_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Move_To_Preset (
      Camera_Queue               : in out PTZ_Optics_Type;
      Preset_ID                  : in     Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img &
         " Wait_Until_Finished " & Wait_Until_Finished'img &
         " Unit_Testing " & Ada_Lib.Unit_Testing'img);

      declare
         Status      : constant Status_Type :=
                        Camera_Queue.Synchronous (Memory_Recall,
                           Options     => ( 1 =>
                                 (
                                    Data           => Data_Type (Preset_ID),
                                    Start          => 6,
                                    Variable_Width => False
                                 )
                              ),
                           Wait_Until_Finished  => Wait_Until_Finished);
      begin
         case Status is
            when Success =>
               Log_Here (Debug, "Synchronous return Success");

            when Standard.Camera.Timeout =>
               Log_Here ("Synchronous return Timeout");
               raise Failed with "Move_To_Preset timed out";

            when Others =>
               declare
                  Message     : constant String :=
                                 "Move_To_Preset return status " &
                                 Status'img;
               begin
                  Log_Here ("Synchronous return not set");
                  raise Failed with "Move_To_Preset returnd not set";
               end;
         end case;
      end;

      Log_Out (Debug);
   end Move_To_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Position_Relative (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
      declare
         Status      : constant Status_Type := Camera.Synchronous (
                        Position_Relative,
                        Options     => (
                           (
                              Data           => Pan_Speed,
                              Start          => 5,
                              Variable_Width => False
                           ),
                           (
                              Data           => Tilt_Speed,
                              Start          => 6,
                              Variable_Width => False
                           ),
                           (
                              Start          => 7,
                              Variable_Width => True,
                              Value          => Convert (Pan),
                              Width          => 4
                           ),
                           (
                              Start          => 11,
                              Variable_Width => True,
                              Value          => Convert (Tilt),
                              Width          => 4
                           )
                        ),
                        Wait_Until_Finished  => True
                     );
      begin
         if Status /= Success then
            raise Failed with "Synchronous failed with " & Status'img;
         end if;
      end;

      Log_Out (Debug);

   exception
      when Fault : others =>
         Trace_Exception (Fault, Here);
         raise;

   end Position_Relative;

   ----------------------------------------------------------------------------
   overriding
   procedure Process_Response (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Video.Lib.Index_Type) is
   ----------------------------------------------------------------------------

      First                      : constant Video.Lib.Index_Type := Response'first;

   begin
      Log_In (Debug, "Response first" & First'img & " last" & Response'last'img);

      if Response (First) = 16#90# then
         for Index in First + 1 .. Response'last loop
            if Response (Index) = 16#FF# then
               Log_Here (Debug, "end response at" & Index'img);
               Next_Buffer_Start := Index + 1;
               Value := Response (First + 1);

               declare
                  Masked_Value   : constant Data_Type := Value and 16#F0#;

               begin
                  case Masked_Value is

                     when 16#40# |
                          16#50# =>
                        Log_Out (Debug, "value " & Image (Value) &
                           " Next_Buffer_Start" & Next_Buffer_Start'img);
                        return;

                     when others =>
                        Log_Exception (Debug);
                        raise Failed with "invalid completed " &
                           Image (Value) & " at " & Here;

                  end case;
               end;
            end if;
         end loop;

         Log_Exception (Debug);
         raise Failed with "invalid completed. FF not found at " &  Here;
      else
         Log_Exception (Debug);
         raise Failed with "invalid completed at " &  Here;
      end if;
   end Process_Response;

   ----------------------------------------------------------------------------
   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "Command " & Command'img);
      Send_Command (Camera, Command, Standard.Camera.Null_Options,
         Get_Ack, Has_Response, Response_Length);
      Log_Out (Debug);
   end Send_Command;

   ----------------------------------------------------------------------------
   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

      Selected_Command           : Standard.Camera.Lib.Base.Command_Type renames
                                    Commands (Command);
      Buffer                     : Video.Lib.Maximum_Command_Type :=
                                    Selected_Command.Command;

   begin
      Log_In (Debug, "Command " & Command'img);
      Standard.Camera.Lib.Base.Check_Command (Command, Selected_Command);
      Standard.Camera.Lib.Base.Apply_Parameters (Buffer, Options);
      if Buffer (Selected_Command.Length) /= 16#FF# then
         raise Failed with "missing end of command at" & Selected_Command.Length'img;
      end if;
      Camera.Write (Buffer ( 1 .. Selected_Command.Length));
      Get_Ack := Selected_Command.Get_Ack;
      Has_Response := Selected_Command.Has_Response;
      Response_Length := Selected_Command.Response_Length;
      Log_Out (Debug, "get ack " & Get_Ack'img &
         " has response " & Has_Response'img);

   exception
      when Fault : others =>
         Log_Exception (Debug, Fault);
         raise;

   end Send_Command;

   ---------------------------------------------------------------
   overriding
   procedure Set_Absolute (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
      declare
         Status         : constant Status_Type := Camera.Synchronous (
                           Position_Absolute,
                           Options     => (
                              (
                                 Data           => Pan_Speed,
                                 Start          => 5,
                                 Variable_Width => False
                              ),
                              (
                                 Data           => Tilt_Speed,
                                 Start          => 6,
                                 Variable_Width => False
                              ),
                              (
                                 Start          => 7,
                                 Variable_Width => True,
                                 Value          => Convert (Pan),
                                 Width          => 4
                              ),
                              (
                                 Start          => 11,
                                 Variable_Width => True,
                                 Value          => Convert (Tilt),
                                 Width          => 4
                              )
                           ),
                           Wait_Until_Finished  => True
                        );
      begin
         if Status /= Success then
            raise Failed with "Synchronous failed with " & Status'img;
         end if;
      end;

--    if Wait_Until_Finished then
--       loop
--          declare
--             Pan                  : Absolute_Type;
--             Tilt                 : Absolute_Type;
--          begin
--             Log_Here (Debug);
--             Camera.Get_Absolute (Pan, Tilt);
--             Log_Here (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
--             exit;
--
--          exception
--             when Fault: others =>
--                Trace_Exception (Debug, Fault);
--             delay 0.5;
--             Log_Here (Debug);
--          end;
--       end loop;
--    end if;
      Log_Out (Debug);
      Log_Out (Debug);
   end Set_Absolute;

   ---------------------------------------------------------------
   overriding
   procedure Set_Power (
      Camera                     : in out PTZ_Optics_Type;
      On                         : in     Boolean) is
   ---------------------------------------------------------------

--    Current_Power              : Boolean;
--    Turning_On                 : Boolean;
--
   begin
      Log_In (Debug, "power " & On'img);
--    Camera.Get_Power (Current_Power);
--    Turning_On := On and not Current_Power;
--    Log_Here (Debug, "Turning_On " & Turning_On'img);
      declare
         Status         : constant Status_Type := Camera.Synchronous (
                           Power_Set,
                           Options     => ( 1 =>
                                 (
                                    Data           => (if On then 2 else 3),
                                    Start          => 5,
                                    Variable_Width => False
                                 )
                              ),
                           Wait_Until_Finished     => False);

      begin
         if Status /= Success then
            Log_Exception (Debug, "could not set power " & On'img);
            raise Failed with "Synchronous failed with " & Status'img;
         end if;
      end;
      if On then -- need to reopen after power comes on
         Log_Here (Debug);
         Put_Line ("wait 90 seconds for camera to reset");
         for Counter in 1 .. 90 loop
            delay (1.0); -- wait for camera to come back on
            Put (Counter'img & " ");
         end loop;
         New_Line;
         Log_Here (Debug);
         Camera.Reopen;
      end if;
      Log_Out (Debug);

   exception

      when Error: others =>
         Trace_Message_Exception (Debug, Error,
            "fault setting power " & On'img);
         raise;

   end Set_Power;

   ---------------------------------------------------------------
   overriding
   procedure Update_Preset (
      Camera_Queue               : in out PTZ_Optics_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img);
      if Ada_Lib.Unit_Testing and then Preset_ID <
            Minimum_Preset_ID_For_Testing then
         raise Failed with "tried to set non testing preset id" &
            Preset_ID'img & ". Less than minimum for testing" &
            Minimum_Preset_ID_For_Testing'img;
      end if;

      case Camera_Queue.Synchronous (Memory_Set,
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Preset_ID),
                  Start          => 6,
                  Variable_Width => False
               )
            ),
            Wait_Until_Finished  => False) is

         when Fault =>
            Log_Here ("Synchronous return fault");

         when Not_Set =>
            Log_Here ("Synchronous return not set");
            raise Failed with "Synchronous command returnd not set";

         when Success =>
            Log_Here (Debug, "Synchronous return Success");
            delay 2.0;  -- camera needs time to update memory

         when Standard.Camera.Timeout =>
            Log_Here ("Synchronous return Timeout");

      end case;

      Log_Out (Debug);
   end Update_Preset;

begin
--Debug := True;
      Log_Here (Debug);
end Camera.Lib.PTZ_Optics;

