--with Ada.Streams;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Hex_IO;
--with Ada_Lib.Socket_IO.Stream_IO;

package body Camera.Commands.PTZ_Optics is

-- use type Ada.Streams.Stream_Element;
   use type Video.Lib.Index_Type;

   Default_Response_Timeout      : constant Duration := 0.6;
   Position_Timeout              : constant Duration := 60.0;
   Power_Inquire_Timeout         : constant Duration := 120.0;
   Commands                      : constant Array (Standard.Camera.Lib.Base.
                                    Commands_Type) of Standard.Camera.Lib.Base.
                                       Command_Type := (
      Standard.Camera.Lib.Base.Auto_Focus           => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#02#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Manual_Focus         => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#03#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Absolute    => ( 15, ( 16#81#,16#01#,16#06#,16#02#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#FF#, others => 0), False, Position_Timeout, False, 3),
      Standard.Camera.Lib.Base.Position_Down_Left   => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#02#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Down_Right  => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#02#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Down        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#02#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Left        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#03#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Relative    => ( 15, ( 16#81#,16#01#,16#06#,16#03#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#FF#, others => 0 ), True, Position_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Request     => ( 5, ( 16#81#,16#09#,16#06#,16#12#,16#FF#, others => 0 ), False, Default_Response_Timeout, True, 11),
      Standard.Camera.Lib.Base.Position_Right       => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#03#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Stop        => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#03#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Up          => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#03#,16#01#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Up_Left     => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#01#,16#01#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Position_Up_Right    => ( 9, ( 16#81#,16#01#,16#06#,16#01#,16#00#,16#00#,16#02#,16#01#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Memory_Recall        => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#02#,16#FF#, others => 0 ), True, Position_Timeout, False, 0),
      Standard.Camera.Lib.Base.Memory_Set           => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#01#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Memory_Reset         => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#00#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Power                => ( 6, ( 16#81#,16#01#,16#04#,16#00#,16#00#,16#FF#, others => 0 ), False, Default_Response_Timeout, true, 3),
      Standard.Camera.Lib.Base.Power_Inquire        => ( 5, ( 16#81#,16#09#,16#04#,16#00#,16#FF#, others => 0 ), False, Power_Inquire_Timeout, True, 4),
      Standard.Camera.Lib.Base.Recall_Speed         => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#02#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Direct          => ( 9, ( 16#81#,16#01#,16#04#,16#47#,16#00#,16#00#,16#00#,16#00#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Stop            => ( 6, ( 16#81#,16#01#,16#04#,16#07#,16#00#,16#FF#, others => 0 ), True, Default_Response_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Inquire         => ( 5, ( 16#81#,16#09#,16#04#,16#47#,16#FF#, others => 0 ), False, Default_Response_Timeout, True, 7),
      Standard.Camera.Lib.Base.Zoom_Tele_Standard   => ( 6, ( 16#81#,16#01#,16#04#,16#07#,16#02#,16#FF#, others => 0 ), True, Position_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Tele_Variable   => ( 6, ( 16#81#,16#01#,16#04#,16#07#,16#20#,16#FF#, others => 0 ), True, Position_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Wide_Standard   => ( 6, ( 16#81#,16#01#,16#04#,16#07#,16#03#,16#FF#, others => 0 ), True, Position_Timeout, False, 0),
      Standard.Camera.Lib.Base.Zoom_Wide_Variable   => ( 6, ( 16#81#,16#01#,16#04#,16#07#,16#30#,16#FF#, others => 0 ), True, Position_Timeout, False, 0)
   );

   ----------------------------------------------------------------------------
   overriding
   procedure Acked (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Video.Lib.Buffer_Type;
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
      Buffer                     : in     Buffer_Type;
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
   function Get_Ack_Length (
      Camera                     : in     PTZ_Optics_Type
   ) return Index_Type is
   ----------------------------------------------------------------------------

   begin
      return 3;
   end Get_Ack_Length;

   ----------------------------------------------------------------------------
   overriding
   function Get_Camera_Speed (
      Camera            : in     PTZ_Optics_Type;
      Which             : in     Which_Speed_Type := Select_Default_Speed
   ) return Data_Type is
   pragma Unreferenced (Camera); -- only used to select camera type
   ----------------------------------------------------------------------------

      Speeds            : constant array (Which_Speed_Type) of Speed_Type := (
                           Select_Minimum_Speed => Minimum_Speed,
                           Select_Default_Speed => Default_Speed,
                           Select_Maximum_Speed => Maximum_Speed);

   begin
      return Speeds (Which);
   end Get_Camera_Speed;

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
   function Get_Maximum_Preset (
      Camera                     : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ----------------------------------------------------------------------------

   begin
      return Maximum_Preset;
   end Get_Maximum_Preset;

   ----------------------------------------------------------------------------
   overriding
   function Get_Timeout (
      Camera                     : in     PTZ_Optics_Type;
      Command                    : in     Standard.Camera.Lib.Base.Commands_Type
   ) return Duration is
   ----------------------------------------------------------------------------

   begin
      return Commands (Command).Response_Timeout;
   end Get_Timeout;

   ----------------------------------------------------------------------------
   overriding
   procedure Process_Response (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Video.Lib.Buffer_Type;
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
      Command                    : in     Standard.Camera.Lib.Base.Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "Command " & Command'img);
      Send_Command (Camera, Command, Standard.Camera.Lib.Base.Null_Option,
         Get_Ack, Has_Response, Response_Length);
      Log_Out (Debug);
   end Send_Command;

   ----------------------------------------------------------------------------
   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in     Standard.Camera.Lib.Base.Commands_Type;
      Options                    : in     Standard.Camera.Lib.Base.Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

      Selected_Command           : Standard.Camera.Lib.Base.Command_Type renames
                                    Commands (Command);
      Buffer                     : Video.Lib.Maximum_Command_Type :=
                                    Selected_Command.Command;

   begin
      Log_In (Debug, "Command " & Command'img);
      Standard.Camera.Lib.Base.Apply_Parameters (Buffer, Options);
      Camera.Write (Buffer ( 1 .. Selected_Command.Length));
      Get_Ack := Selected_Command.Get_Ack;
      Has_Response := Selected_Command.Has_Response;
      Response_Length := Selected_Command.Response_Length;
      Log_Out (Debug, "get ack " & Get_Ack'img &
         " has response " & Has_Response'img);
   end Send_Command;

begin
--Debug := True;
      Log_Here (Debug);
end Camera.Commands.PTZ_Optics;

