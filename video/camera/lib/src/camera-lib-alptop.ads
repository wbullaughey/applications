with Camera.Lib.Base;
with Configuration.Camera;

package Camera.LIB.ALPTOP is

   type ALPTOP_Type is new Lib.Base.Base_Camera_Type with null record;

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
   function Get_Ack_Length (
      Camera                     : in     ALPTOP_Type
   ) return Index_Type;

   overriding
   function Get_Default_Preset (
      Camera                     : in     ALPTOP_Type
   ) return Configuration.Camera.Preset_ID_Type;

   overriding
   function Get_Timeout (
      Camera                     : in     ALPTOP_Type;
      Command                    : in     Commands_Type
   ) return Duration;

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

end Camera.LIB.ALPTOP;
