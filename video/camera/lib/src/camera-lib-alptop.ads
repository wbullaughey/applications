with Camera.Commands;
--with Camera.Lib.Base;
with Configuration.Camera;

package Camera.LIB.ALPTOP is

   type ALPTOP_Type is new Camera.Commands.Camera_Queue_Type with null record;

private

   overriding
   procedure Acked (
      Camera                     : in     ALPTOP_Type;
      Response                   : in     Response_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Video.Lib.Index_Type);

-- overriding
-- procedure Cell_Preset (
--    Camera                     : in out ALPTOP_Type;
--    Preset                     : in     Video.Lib.Camera_Preset_Type);

   overriding
   procedure Completed (
      Camera                     : in     ALPTOP_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type);

   overriding
   procedure Get_Absolute (
      Camera                     : in out ALPTOP_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type);

   overriding
   function Get_Ack_Length (
      Camera                     : in     ALPTOP_Type
   ) return Index_Type;

   overriding
   function Get_Default_Preset (
      Camera                     : in     ALPTOP_Type
   ) return Configuration.Camera.Preset_ID_Type;

   overriding
   procedure Get_Power (
      Camera                     : in out ALPTOP_Type;
      Power                      :    out Boolean);

   overriding
   function Get_Timeout (
      Camera                     : in     ALPTOP_Type;
      Command                    : in     Commands_Type
   ) return Duration;

   overriding
   procedure Get_Zoom (
      Camera                     : in out ALPTOP_Type;
      Zoom                       :    out Absolute_Type);

   overriding
   procedure Position_Relative (
      Camera                     : in out ALPTOP_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   overriding
   procedure Process_Response (
      Camera                     : in     ALPTOP_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Video.Lib.Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out ALPTOP_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

   overriding
   procedure Send_Command (
      Camera                     : in out ALPTOP_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type);

   overriding
   procedure Set_Absolute (
      Camera                     : in out ALPTOP_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   overriding
   procedure Set_Power (
      Camera                     : in out ALPTOP_Type;
      On                         : in     Boolean);

   -- sets camera to a preset
   overriding
   procedure Set_Preset (
      Camera                     : in out ALPTOP_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

end Camera.LIB.ALPTOP;
