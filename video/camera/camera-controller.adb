with Camera.Commands;

package body Camera.Controller is

   ---------------------------------------------------------------
   procedure Move_Relative (
      Controller                 : in out Controller_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type) is
   ---------------------------------------------------------------

   begin
      if Controller.Enabled then
         Controller.Camera.Position_Relative (Pan, Tilt,
            Controller.Pan_Speed, Controller.Tilt_Speed);
      end if;
   end Move_Relative;

   ---------------------------------------------------------------
   procedure Set_Enable (
      Controller                 : in out Controller_Type;
      State                      : in     Boolean) is
   ---------------------------------------------------------------

   begin
      Controller.Enabled := State;
   end Set_Enable;

   ---------------------------------------------------------------
   procedure Set_Preset (
      Controller                 : in out Controller_Type;
      Preset_ID                  : in     Camera_Preset_Type) is
   ---------------------------------------------------------------

   begin
      if Controller.Enabled then
         Controller.Camera.Set_Preset (Preset_ID);
      end if;
   end Set_Preset;

   ---------------------------------------------------------------
   procedure Set_Speed (
      Controller                 : in out Controller_Type;
      Pan_Speed                  : in      Property_Type;
      Tilt_Speed                 : in      Property_Type) is
   ---------------------------------------------------------------

   begin
      Controller.Pan_Speed := Pan_Speed;
      Controller.Tilt_Speed := Tilt_Speed;
   end Set_Speed;

   ---------------------------------------------------------------
   procedure Set_Zoom (
      Controller                 : in out Controller_Type;
      Zoom                       : in     Zoom_Type) is
   ---------------------------------------------------------------

   begin
      if Controller.Enabled then
         Controller.Camera.Set_Zoom (Zoom);
      end if;
   end Set_Zoom;

end Camera.Controller;
