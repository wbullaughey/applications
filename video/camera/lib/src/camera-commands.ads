--with AUnit.Test_Suites;
with Ada_Lib.Strings;
with Ada_Lib.Time;
--with Configuration.Camera;
with Camera.Lib.Base;
--with Interfaces;

package Camera.Commands is

   Failed                        : exception;
   Timeout                       : exception;

   type Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new Standard.Camera.Lib.Base.
                                    Base_Camera_Type (
                                       Description) with null record;

   type Camera_Class_Access      is access all Camera_Type'class;

   type Which_Speed_Type         is (
                                    Select_Minimum_Speed,
                                    Select_Default_Speed,
                                    Select_Maximum_Speed);

   type Zoom_Mode_Type          is (Minimum, Maximum);

   Wait_Until_Finished_Time     : constant := 60.0;

   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time);

   function Get_Camera_Speed (
      Camera            : in     Camera_Type;
      Which             : in     Which_Speed_Type := Select_Default_Speed
   ) return Data_Type is abstract;

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
      Preset_ID                  : in     Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True;
      Speed                      : in     Property_Type := 0);  -- 0 => default

   procedure Set_Preset_Speed (
      Camera                     : in out Camera_Type;
      Speed                      : in    Property_Type);

   procedure Set_Variable_Zoom (
      Camera                     : in out Camera_Type;
      Mode                       : in     Zoom_Mode_Type;
      Value                      : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True);

   Debug                         : Boolean := False;

end Camera.Commands;
