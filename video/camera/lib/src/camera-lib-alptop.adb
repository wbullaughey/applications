with Ada.Streams;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Base;
with Hex_IO;

package body Camera.LIB.ALPTOP is

   use type Ada.Streams.Stream_Element;
   use type Interfaces.Unsigned_16;
   use type Video.Lib.Index_Type;

   Timeout                       : constant Duration := 0.5;
   Commands                      : constant Array (Commands_Type) of
                                    Camera.Lib.Base.Command_Type := (
         Auto_Focus     => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#02#,16#FF#, others => 0 ), None, Timeout, False, 0),
         Manual_Focus   => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#03#,16#FF#, others => 0 ), None, Timeout, False, 0),
         Memory_Recall         => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#1#,16#FF#, others => 0 ), None, Timeout, False, 0),
         others         => ( 1, ( others => 0), None, 0.0, False, 0)
      );

   ----------------------------------------------------------------------------
   overriding
   procedure Acked (
      Camera                     : in     ALPTOP_Type;
      Response                   : in     Response_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type) is
   pragma Unreferenced (Camera, Response, Value, Next_Buffer_Index);
   ----------------------------------------------------------------------------

   begin
      raise Failed with "not implemented";
   end Acked;

   ----------------------------------------------------------------------------
   overriding
   procedure Completed (
      Camera                     : in     ALPTOP_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type) is
   pragma Unreferenced (Buffer, Camera, Completion_Value, Next_Byte, Start);
   ----------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Completed;

   ----------------------------------------------------------------------------
   overriding
   procedure Get_Absolute (
      Camera                     : in out ALPTOP_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      In_Queue                   : in     Boolean := False) is
   ----------------------------------------------------------------------------

      Accumulator                : Interfaces.Unsigned_16;
      Conversion                 : Absolute_Type;
      for Conversion'address use Accumulator'address;
      Response_Buffer            : Maximum_Response_Type;
      Response_Length            : Index_Type;

   begin
      Log_In (Debug);
      Camera.Process_Command (Position_Request,
         Options                 => Null_Options,
         Response                => Response_Buffer,
         Response_Length         => Response_Length,
         In_Queue                => False);

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
   end Get_Absolute;

   ----------------------------------------------------------------------------
   overriding
   function Get_Ack_Length (
      Camera                     : in     ALPTOP_Type
   ) return Index_Type is
   ----------------------------------------------------------------------------

   begin
      return 3;
   end Get_Ack_Length;

   ----------------------------------------------------------------------------
   overriding
   function Get_Default_Preset (
      Camera                     : in     ALPTOP_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ----------------------------------------------------------------------------

   begin
      return 0;
   end Get_Default_Preset;

   ----------------------------------------------------------------------------
   overriding
   procedure Get_Power (
      Camera                     : in out ALPTOP_Type;
      Power                      :    out Boolean) is
   pragma Unreferenced (Camera, Power);
   ----------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Get_Power;

   ----------------------------------------------------------------------------
   overriding
   function Get_Timeout (
      Camera                     : in     ALPTOP_Type;
      Command                    : in     Commands_Type
   ) return Duration is
   pragma Unreferenced (Camera, Command);
   ----------------------------------------------------------------------------

   begin
      Not_Implemented;
      return 0.0;
   end Get_Timeout;

   ---------------------------------------------------------------
   overriding
   procedure Get_Zoom (
      Camera                     : in out ALPTOP_Type;
      Zoom                       :    out Absolute_Type) is
      pragma Unreferenced (Camera, Zoom);
   ---------------------------------------------------------------

   begin
      Not_Implemented;
   end Get_Zoom;

   ---------------------------------------------------------------
   overriding
   function Last_Preset (
      Camera_Queue               : in     ALPTOP_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ---------------------------------------------------------------

   begin
      Not_Implemented;
      return 0;
   end Last_Preset;

   ---------------------------------------------------------------
   overriding
   function Minimum_Test_Preset (
      Camera_Queue               : in     ALPTOP_Type
   ) return Configuration.Camera.Preset_ID_Type is
   ---------------------------------------------------------------

   begin
      Not_Implemented;
      return 0;
   end Minimum_Test_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Move_To_Preset (
      Camera_Queue               : in out ALPTOP_Type;
      Preset_ID                  : in     Preset_ID_Type) is
   pragma Unreferenced (Camera_Queue, Preset_ID, In_Queue);
   ---------------------------------------------------------------

   begin
      Not_Implemented;
   end Move_To_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Position_Relative (
      Camera                     : in out ALPTOP_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is
   pragma Unreferenced (Camera, Pan, Tilt, Pan_Speed, Tilt_Speed);
   ---------------------------------------------------------------

   begin
      Not_Implemented;
   end Position_Relative;

   ----------------------------------------------------------------------------
   overriding
   procedure Process_Response (
      Camera                     : in     ALPTOP_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Video.Lib.Index_Type) is
   pragma Unreferenced (Camera, Next_Buffer_Start, Response, Value);
   ----------------------------------------------------------------------------

   begin
      raise Failed with "not implemented";
   end Process_Response;

   ----------------------------------------------------------------------------
   overriding
   procedure Send_Command (
      Camera                     : in out ALPTOP_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "Command " & Command'img);
      Send_Command (Camera, Command,
         Standard.Camera.Null_Options, Get_Ack, Has_Response,
         Response_Length);
      Log_Out (Debug);
   end Send_Command;

   ----------------------------------------------------------------------------
   overriding
   procedure Send_Command (
      Camera                     : in out ALPTOP_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

      Buffer                     : Video.Lib.Maximum_Command_Type :=
                                    Commands (Command).Command;
      Selected_Command           : Standard.Camera.Lib.Base.Command_Type renames
                                    Commands (Command);

   begin
      Log_In (Debug, "Command " & Command'img);
      Standard.Camera.Lib.Base.Check_Command (Command, Selected_Command);
      for Index in Options'range loop
         Buffer (Options (Index).Start) := Options (Index).Data;
      end loop;

      if Debug then
         Hex_IO.Dump_8 (Buffer'address, Natural (Selected_Command.Length) * 8, 32);
      end if;

      Camera.Write (Buffer (1 .. Selected_Command.Length));
      Get_Ack := None;
      Has_Response := False;
      Response_Length := 0;
      Log_Out (Debug);
   end Send_Command;

   ---------------------------------------------------------------
   overriding
   procedure Set_Absolute (
      Camera                     : in out ALPTOP_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is
   pragma Unreferenced (Camera, Pan, Tilt, Pan_Speed, Tilt_Speed);
   ---------------------------------------------------------------

   begin
      Not_Implemented;
   end Set_Absolute;

   ---------------------------------------------------------------
   overriding
   procedure Set_Power (
      Camera                     : in out ALPTOP_Type;
      On                         : in     Boolean) is
   pragma Unreferenced (Camera, On);
   ---------------------------------------------------------------

   begin
not_implemented;
   end Set_Power;

   ---------------------------------------------------------------
   overriding
   procedure Update_Preset (
      Camera                     : in out ALPTOP_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type) is
   pragma Unreferenced (Camera, Preset_ID);
   ---------------------------------------------------------------

   begin
      Not_Implemented;
   end Update_Preset;

begin
   -- Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Camera.LIB.ALPTOP;
