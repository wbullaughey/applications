with Camera.Commands;
with Camera.Lib.Base;
with Configuration.Camera;

package Camera.Lib.PTZ_Optics is

   subtype Ack_Type              is Response_Type (1 .. 3);

   type PTZ_Optics_Type is new Camera.Commands.Camera_Queue_Type with null record;

   Last_Preset                   : constant := 127;
   Port                          : constant := 5678;
   Powerup_Preset                : constant Configuration.Camera.Preset_ID_Type := 0;

private

   overriding
   procedure Acked (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Response_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type);

   overriding
   procedure Completed (
      Camera                     : in     PTZ_Optics_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type);

   overriding
   procedure Get_Absolute (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type);

   overriding
   function Get_Ack_Length (
      Camera                     : in     PTZ_Optics_Type
   ) return Index_Type;

   overriding
   function Get_Default_Preset (
      Camera                     : in     PTZ_Optics_Type
   ) return Configuration.Camera.Preset_ID_Type;

   overriding
   function Get_Timeout (
      Camera                     : in     PTZ_Optics_Type;
      Command                    : in     Commands_Type
   ) return Duration;

   overriding
   procedure Get_Zoom (
      Camera                     : in out PTZ_Optics_Type;
      Zoom                       :    out Absolute_Type);

   overriding
   procedure Position_Relative (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   overriding
   procedure Process_Response (
      Camera                     : in     PTZ_Optics_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Video.Lib.Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in    Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out PTZ_Optics_Type;
      Command                    : in    Commands_Type;
      Options                    : in    Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

   overriding
   procedure Set_Absolute (
      Camera                     : in out PTZ_Optics_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   -- sets camera to a preset
   overriding
   procedure Set_Preset (
      Camera                     : in out PTZ_Optics_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

end Camera.Lib.PTZ_Optics;
