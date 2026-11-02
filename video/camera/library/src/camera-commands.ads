--with AUnit.Test_Suites;
--with Ada_lib.Lock;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Time;
--with Video.Object.Camera;
with Camera.Lib.Base;
--with Interfaces;

package Camera.Commands is

   Failed                        : exception;
   Invalid_Parameter             : exception;
   Timeout                       : exception;

   type Which_Speed_Type         is (
                                    Select_Minimum_Speed,
                                    Select_Default_Speed,
                                    Select_Maximum_Speed);

   type Zoom_Mode_Type          is (Minimum, Maximum);

   type Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new Lib.Base.Base_Camera_Type with private;

   type Camera_Access            is access all Camera_Type;
   type Camera_Class_Access      is access all Camera_Type'class;

   overriding
   procedure Get_Absolute (
      Camera            : in out Camera_Type;
      Pan               :    out Absolute_Type;
      Tilt              :    out Absolute_Type;
      Stabalize_Time    : in     Ada_Lib.Time.Duration_Type :=
                                    Video.Lib.Wait_Until_Finished_Time);

   function Get_Brand (
      Camera            : in     Camera_Type
   ) return Brand_Type
   with Pre => Has_Brand (Camera);

   overriding
   function Get_Brand_Name (
      Camera            : in     Camera_Type
   ) return String
   with Pre => Has_Brand_Name (Camera);

   function Get_Camera_Speed_Range (
      Camera            : in     Camera_Type;
      Which             : in     Which_Speed_Type := Select_Default_Speed
   ) return Data_Type is abstract;

   function Get_Default_Preset_Number (
      PTZ_Optics        : in     Camera_Type   -- only ussed for dispatch
   ) return Video.Lib.Preset_ID_Type is abstract;

   function Get_Default_Speed (
      Camera            : in     Camera_Type
   ) return Property_Type is abstract;

   function Get_Power (
      Camera                     : in out Camera_Type
   ) return Boolean;

   procedure Get_Zoom (
      Camera                     : in out Camera_Type;
      Zoom                       :    out Absolute_Type);

   function Has_Brand (
      Camera            : in     Camera_Type
   ) return Boolean;

   function Has_Brand_Name (
      Camera            : in     Camera_Type
   ) return Boolean;

   procedure Position_Relative (
      Camera                     : in out Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Wait_For_Complete          : in      Boolean := True;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   overriding
   procedure Relative_Command (
      Camera                     : in out Camera_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Default_Preset_Number (
      PTZ_Optics  : in     Camera_Type; -- only ussed for dispatch
      Camera_ID         : in     Video.Lib.Camera_ID_Type) is abstract;

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
      Preset_ID                  : in     Video.Lib.Preset_ID_Type;
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

private

   Lock_Description              : aliased String := "camera lock";

   type Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new Standard.Camera.Lib.Base.
                                    Base_Camera_Type with record
      Brand                      : Brand_Type := No_Camera;
      Brand_Name                 : Ada_Lib.Strings.Unlimited.String_Type;
      Current_Speed              : Property_Type := 0;   -- not set yet
--    Lock                       : Ada_lib.Lock.Lock_Type (
--                                  Lock_Description'access);
   end record;



end Camera.Commands;
