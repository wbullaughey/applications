with AUnit.Test_Suites;

package Configuration.Camera.State.Unit_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
-- with Pre => Standard.Camera.Lib.Global_Camera_Lib_Options /= Null;

   Debug                         : Boolean := False;

end Configuration.Camera.State.Unit_Tests;
