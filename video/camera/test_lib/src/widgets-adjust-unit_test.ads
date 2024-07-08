with AUnit.Test_Suites;

package Widgets.Adjust.Unit_Test is

   Failed                        : exception;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   Debug                         : aliased Boolean := False;

end Widgets.Adjust.Unit_Test;



