with Camera.Lib.Base;

package Camera.Controller is

   type Controller_Type          is tagged private;

   procedure Move_Relative (
      Controller                 : in out Controller_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type);

   procedure Set_Enable (
      Controller                 : in out Controller_Type;
      State                      : in     Boolean);

   procedure Set_Preset (
      Controller                 : in out Controller_Type;
      Preset_ID                  : in     Camera_Preset_Type);

   procedure Set_Speed (
      Controller                 : in out Controller_Type;
      Pan_Speed                  : in      Property_Type;
      Tilt_Speed                 : in      Property_Type);

   procedure Set_Zoom (
      Controller                 : in out Controller_Type;
      Zoom                       : in     Zoom_Type);

private

   type Controller_Type          is tagged record
      Camera                     : Lib.Base.Base_Camera_Class_Access;
      Enabled                    : Boolean := False;
      Pan_Speed                  : Property_Type := 1;
      Tilt_Speed                 : Property_Type := 1);
   end record


end Camera.Controller;
