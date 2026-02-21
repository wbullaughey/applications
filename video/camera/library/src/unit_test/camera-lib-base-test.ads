--with Ada_Lib.Options.Flags;
with AUnit.Test_Suites;

package Camera.Lib.Base.Test is

   Failed                        : exception;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Have_Ada_Lib_Program_Options;

end Camera.Lib.Base.Test;
