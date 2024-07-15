--with AUnit.Test_Suites;
with Configuration.Camera;
with Camera.Lib.Base;
with Interfaces;
with Video.Lib;

package Camera.Commands is

   Failed                        : exception;

-- use type Data_Type;
-- use type Value_Type;

   subtype Absolute_Type         is Interfaces.Integer_16;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   subtype Relative_Type         is Video.Lib.Relative_Type range -2**15 .. 2**15;

   type Camera_Type              is abstract new Standard.Camera.Lib.Base.Base_Camera_Type with
                                    null record;

   type Camera_Class_Access      is access all Camera_Type'class;

   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type);

   procedure Get_Zoom (
      Camera                     : in out Camera_Type;
      Zoom                       :    out Absolute_Type);

   procedure Position_Relative (
      Camera                     : in out Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   procedure Set_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Power (
      Camera                     : in out Camera_Type;
      On                         : in     Boolean);

   -- sets camera to a preset
   procedure Set_Preset (
      Camera                     : in out Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

   Debug                         : Boolean := False;

end Camera.Commands;
