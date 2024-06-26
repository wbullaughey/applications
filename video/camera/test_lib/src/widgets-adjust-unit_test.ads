with Ada_Lib.Strings;
--with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Timer;
--with ADA_LIB.Trace;
with AUnit.Test_Suites;
with Base;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;
with Events;

package Widgets.Adjust.Unit_Test is

   Failed                        : exception;

   type Widgets_Adjust_Test_Type (
      Brand                      : Camera.Lib.Brand_Type) is new
                                    Camera.Lib.Unit_Test.Camera_Window_Test_Type (
                                       Initialize_GNOGA  => True,
                                       Run_Main          => True) with record
      Setup                      : Configuration.Camera.Setup.Setup_Type;
   end record;

   type Widgets_Adjust_Test_Access
                                 is access Widgets_Adjust_Test_Type;

   overriding
   function Name (
      Test                       : in     Widgets_Adjust_Test_Type
   ) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Widgets_Adjust_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Widgets_Adjust_Test_Type
   ) with Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (
      Test                       : in out Widgets_Adjust_Test_Type);

   Debug                         : aliased Boolean := False;
   Suite_Name                   : constant String := "Adjust_Card";

private

   type Mouse_Move_Event_Type (
      Description_Pointer  : Ada_Lib.Strings.String_Constant_Access := Null;
      Offset               : Ada_Lib.Timer.Duration_Access := Null
   ) is new Events.Button_Push_Event_Type (
         Description_Pointer  => Description_Pointer,
         Offset               => Offset) with record
      Mouse_Event                : Gnoga.Gui.Base.Mouse_Event_Record;
   end record;

   procedure Initialize_Event (
      Mouse_Move_Evet         : in out Mouse_Move_Event_Type;
      Connection_Data         : in     Base.Connection_Data_Class_Access;
      Description             : in     String;
      Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record;
      Wait                    : in     Duration);

   overriding
   procedure Callback (
      Event                   : in out Mouse_Move_Event_Type);

end Widgets.Adjust.Unit_Test;



