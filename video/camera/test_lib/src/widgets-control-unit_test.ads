--with Ada_Lib.GNOGA.Unit_Test;
with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;

package Widgets.Control.Unit_Test is

   type Test_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type;
      Initialize_GNOGA           : Boolean) is new Camera.Lib.Unit_Test.
                                    Camera_Window_Test_With_Camera_Type (
                                       Brand             => Brand,
                                       Initialize_GNOGA  => Initialize_GNOGA,
                                       Run_Main          => True) with record
      Setup                      : Configuration.Camera.Setup.Setup_Type;
   end record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type);

   procedure Test_Create_Control (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Control";

end Widgets.Control.Unit_Test;

