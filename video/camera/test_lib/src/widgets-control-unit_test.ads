--with Ada_Lib.GNOGA.Unit_Test;
with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;

package Widgets.Control.Unit_Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;

end Widgets.Control.Unit_Test;

