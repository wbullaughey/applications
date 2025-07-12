with Ada_Lib.Strings;
with Camera.Lib.Base;
with Configuration.Camera;

package Camera.Commands.PTZ_Optics is

   Invalid_Command               : exception;

   subtype Ack_Type              is Buffer_Type (1 .. 3);

   type PTZ_Optics_Type(
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is new Standard.Camera.Commands.Camera_Type (
      Description) with null record;

   Default_Read_Timeout          : constant Ada_Lib.Socket_IO.Timeout_Type := 0.2;
   Default_Write_Timeout         : constant Ada_Lib.Socket_IO.Timeout_Type := 0.2;

   Maximum_Preset                   : constant := 256;   -- standard set to same as preset 0
   Port                          : constant := 5678;
   Powerup_Preset                : constant Configuration.Camera.Preset_ID_Type := 0;

private

   overriding
   procedure Acked (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Video.Lib.Buffer_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type);

   overriding
   procedure Completed (
      Camera                     : in     PTZ_Optics_Type;
      Buffer                     : in     Buffer_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type);

   overriding
   function Get_Ack_Length (
      Camera                     : in     PTZ_Optics_Type
   ) return Index_Type;

   overriding
   function Get_Default_Preset (
      Camera                     : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type;

   overriding
   function Get_Maximum_Preset (
      Camera                     : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type;

   overriding
   function Get_Timeout (
      Camera                     : in     PTZ_Optics_Type;
      Command                    : in     Standard.Camera.Lib.Base.Commands_Type
   ) return Duration;

   overriding
   procedure Process_Response (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Video.Lib.Buffer_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Video.Lib.Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in    Standard.Camera.Lib.Base.Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in    Standard.Camera.Lib.Base.Commands_Type;
      Options                    : in    Standard.Camera.Lib.Base.Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

end Camera.Commands.PTZ_Optics;
