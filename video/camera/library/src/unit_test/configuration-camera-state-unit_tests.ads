with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;

package Configuration.Camera.State.Unit_Tests is

   use type Standard.Camera.Lib.Unit_Test.
      Unit_Test_Options_Constant_Class_Access;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Standard.Camera.Lib.Unit_Test.Unit_Test_Options /= Null;

   Debug                         : Boolean := False;

end Configuration.Camera.State.Unit_Tests;
