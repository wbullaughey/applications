with Ada_Lib.Options.Verification;
with AUnit.Test_Suites;

package Configuration.Camera.State.Unit_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Verification.Have_Ada_Lib_Verification_Options;

end Configuration.Camera.State.Unit_Tests;
