--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Suites;
--with Video.Lib;

package Camera.Lib.Base.Command_Tests is

   Failed                        : exception;

   use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options /= Null;
   Debug                         : Boolean := False;

end Camera.Lib.Base.Command_Tests;
