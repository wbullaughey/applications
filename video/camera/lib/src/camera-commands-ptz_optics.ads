with Ada_Lib.Strings;
with Camera.Lib.Base;
--with Configuration.Camera;
with Video.Lib;
package Camera.Commands.PTZ_Optics is

   Invalid_Command               : exception;

   Minimum_Speed                 : constant := 16#1#;
   Maximum_Speed                 : constant := 16#18#;

   subtype Ack_Type              is Buffer_Type (1 .. 3);
   subtype Speed_Type            is Property_Type range Minimum_Speed .. Maximum_Speed;

   use type Speed_Type;

   type PTZ_Optics_Type(
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is new Standard.Camera.Commands.Camera_Type (
      Description) with null record;

   Default_Read_Timeout          : constant Ada_Lib.Socket_IO.Timeout_Type := 0.2;
   Default_Speed                 : constant Speed_Type := (Maximum_Speed -
                                    Minimum_Speed) /2;
   Default_Write_Timeout         : constant Ada_Lib.Socket_IO.Timeout_Type := 0.2;

   First_Preset_Number           : constant := 0;
   Default_Preset_Number         : constant := First_Preset_Number;
   Maximum_Preset_Number         : constant := 254;
   Port                          : constant := 5678;
   Power_On_Preset_Number        : constant := First_Preset_Number;

private

   overriding
   procedure Acked (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Video.Lib.Buffer_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type);

-- function Allocate_Preset_ID (
--    Camera                     : in     PTZ_Optics_Type;
--    ID                         : in     Preset_Range_Type
-- ) return Preset_ID_Type;

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
   function Get_Camera_Speed (
      Camera                     : in     PTZ_Optics_Type;
      Which                      : in     Which_Speed_Type := Select_Default_Speed
   ) return Data_Type;

   overriding
   function Get_Timeout (
      Camera                     : in     PTZ_Optics_Type;
      Command                    : in     Standard.Camera.Lib.Base.Commands_Type
   ) return Duration;

   overriding
   procedure Initialize_Standard_Preset_IDs (
      Camera                     : in     PTZ_Optics_Type);

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
