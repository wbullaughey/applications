with AUnit.Test_Cases;
with AUnit.Test_Suites;
--with Camera.Lib.Unit_Test;
--with Ada_Lib.GNOGA.Unit_Test;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;
--with Configuration.Camera.State;

package Main.Unit_Test is

   Failed                        : exception;

   type Test_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type) is new
                                    Camera.Lib.Unit_Test.
                                       Camera_Window_Test_With_Camera_Type (
                                          Brand             => Brand,
                                          Initialize_GNOGA  => True,
                                          Run_Main          => False) with record
      Setup                      : Configuration.Camera.Setup.Setup_Type;
--    State                      : aliased Configuration.Camera.State.State_Type;
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
   ) with Pre => not Test.Verify_Set_Up,
          Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type);

   procedure Test_Halt (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Preset_Library (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Main";

end Main.Unit_Test;
