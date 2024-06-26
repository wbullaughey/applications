with Ada_Lib.Options;
with Ada_Lib.Unit_Test.Tests;
with AUnit.Test_Suites;

package Camera.Commands.Unit_Test is

   Failed                        : exception;

   type Test_Type                is new Ada_Lib.Unit_Test.Tests.Test_Case_Type
                                    with record
      Camera                     : Camera_Class_Access := Null;
   end record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type
   ) with Post => Test.Verify_Set_Up;

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Have_Options;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type
   ) with post => Verify_Torn_Down (Test);

   Debug                         : Boolean := False;
   Suite_Name                    : constant String := "Configured";

end Camera.Commands.Unit_Test;
