--with Ada.Streams;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Lib.Base;
with Hex_IO;

package body Camera.LIB.ALPTOP is

   use type Video.Lib.Index_Type;

   Timeout                       : constant Duration := 0.5;
   Commands                      : constant Array (Commands_Type) of
                                    Camera.Lib.Base.Command_Type := (
         Auto_Focus     => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#02#,16#FF#, others => 0 ), False, Timeout, False, 0),
         Manual_Focus   => ( 6, ( 16#81#,16#01#,16#04#,16#38#,16#03#,16#FF#, others => 0 ), False, Timeout, False, 0),
         Memory_Recall         => ( 7, ( 16#81#,16#01#,16#04#,16#3F#,16#02#,16#1#,16#FF#, others => 0 ), False, Timeout, False, 0),
         others         => ( 1, ( others => 0), False, 0.0, False, 0)
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
      Get_Ack                    :    out Boolean;
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
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is
   ----------------------------------------------------------------------------

      Buffer                     : Video.Lib.Maximum_Command_Type :=
                                    Commands (Command).Command;
      Selected_Command           : Standard.Camera.Lib.Base.Command_Type renames
                                    Commands (Command);

   begin
      Log_In (Debug, "Command " & Command'img);
      for Index in Options'range loop
         Buffer (Options (Index).Start) := Options (Index).Data;
      end loop;

      if Debug then
         Hex_IO.Dump_8 (Buffer'address, Natural (Selected_Command.Length) * 8, 32);
      end if;

      Camera.Write (Buffer (1 .. Selected_Command.Length));
      Get_Ack := False;
      Has_Response := False;
      Response_Length := 0;
      Log_Out (Debug);
   end Send_Command;

end Camera.LIB.ALPTOP;
