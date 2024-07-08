with Ada_Lib.Options;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;

package Widgets.Configured.Unit_Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Have_Options;

   Debug                         : Boolean := False;
end Widgets.Configured.Unit_Test;

