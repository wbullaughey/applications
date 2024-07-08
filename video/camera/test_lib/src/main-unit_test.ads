with AUnit.Test_Suites;

package Main.Unit_Test is

   Failed                        : exception;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : Boolean := False;
end Main.Unit_Test;
