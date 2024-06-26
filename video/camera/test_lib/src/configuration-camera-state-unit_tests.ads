with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;

package Configuration.Camera.State.Unit_Tests is

   type Configuration_Tests_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type) is new
                                    Standard.Camera.Lib.Unit_Test.
                                    Camera_Test_Type (Brand) with null record;

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
-- with Pre => Standard.Camera.Lib.Global_Camera_Lib_Options /= Null;

   overriding
   procedure Tear_Down (
      Test                       : in out Configuration_Tests_Type);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "State";

end Configuration.Camera.State.Unit_Tests;
