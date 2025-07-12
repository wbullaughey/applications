--with AUnit.Test_Suites;
with Ada_Lib.Strings;
with Ada_Lib.Time;
with Configuration.Camera;
with Camera.Lib.Base;
with Interfaces;
with Video.Lib;

package Camera.Commands is

   Failed                        : exception;
   Timeout                       : exception;

-- use type Data_Type;
-- use type Value_Type;

   subtype Absolute_Type         is Interfaces.Integer_16;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   subtype Relative_Type         is Video.Lib.Relative_Type range -2**15 .. 2**15;

   type Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new Standard.Camera.Lib.Base.
                                    Base_Camera_Type (
                                       Description) with null record;

   type Camera_Class_Access      is access all Camera_Type'class;

   type Zoom_Mode_Type          is (Minimum, Maximum);

   Wait_Until_Finished_Time     : constant := 60.0;

   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time);

   function Get_Power (
      Camera                     : in out Camera_Type
   ) return Boolean;

   procedure Get_Zoom (
      Camera                     : in out Camera_Type;
      Zoom                       :    out Absolute_Type);

   procedure Position_Relative (
      Camera                     : in out Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Wait_For_Complete          : in      Boolean := True;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   procedure Set_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Direct_Zoom (
      Camera                     : in out Camera_Type;
      Value                      : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True);

   procedure Set_Fixed_Zoom (
      Camera                     : in out Camera_Type;
      Mode                       : in     Zoom_Mode_Type;
      Wait_For_Complete          : in     Boolean := True);

   procedure Set_Power (
      Camera                     : in out Camera_Type;
      On                         : in     Boolean);

   -- sets camera to a preset
   procedure Set_Preset (
      Camera                     : in out Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

   procedure Set_Variable_Zoom (
      Camera                     : in out Camera_Type;
      Mode                       : in     Zoom_Mode_Type;
      Value                      : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True);

   Debug                         : Boolean := False;

end Camera.Commands;
