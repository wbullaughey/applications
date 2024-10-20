with Ada_Lib.Options;
with AUnit.Test_Suites;

package Camera.Commands.Unit_Test is

   Failed                        : exception;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Have_Options;

   Debug                         : Boolean := False;

end Camera.Commands.Unit_Test;
