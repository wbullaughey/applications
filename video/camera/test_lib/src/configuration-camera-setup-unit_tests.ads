with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;
--with Configuration.Camera.State;
--with Runtime_Options;
--with Gnoga.Gui.Base;
--with Gnoga.GUI.Window;

package Configuration.Camera.Setup.Unit_Tests is

   type Configuration_Tests_Type is new Standard.Camera.Lib.
                                    Unit_Test.Camera_Test_Type with record
      Setup                      : Configuration.Camera.Setup.Setup_Type;
   end record;

   type Configuration_Tests_Access is access Configuration_Tests_Type;

   overriding
   function Name (
      Test                       : in     Configuration_Tests_Type
   ) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Tests_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Configuration_Tests_Type
   ) with Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   overriding
   procedure Tear_Down (
      Test                       : in out Configuration_Tests_Type);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Setup";

end Configuration.Camera.Setup.Unit_Tests;
