with Ada_Lib.Options;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;

package Widgets.Configured.Unit_Test is

   type Test_Type (
      Initialize_GNOGA           : Boolean) is new
                                    Camera.Lib.Unit_Test.Camera_Window_Test_Type (
                                       Initialize_GNOGA,
                                       Run_Main => True) with record
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

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Have_Options;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type
   ) with post => Verify_Torn_Down (Test);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Configured";

end Widgets.Configured.Unit_Test;

