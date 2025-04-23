--
--  Copyright (C) 2008, AdaCore
--
with Ada_Lib.Options.Actual;
with AUnit.Test_Suites;
--with Video.Lib;

package Camera.Lib.Base.Command_Tests is

   Failed                        : exception;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;
   Debug                         : Boolean := False;

end Camera.Lib.Base.Command_Tests;
