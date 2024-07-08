with AUnit.Test_Suites;

package Camera.Lib.Base.Test is

   Failed                        : exception;

   use type Ada_Lib.Options_Interface.Interface_Options_Constant_Class_Access;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options_Interface.Read_Only_Options /= Null;

end Camera.Lib.Base.Test;
