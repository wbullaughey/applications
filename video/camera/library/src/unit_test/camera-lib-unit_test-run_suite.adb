with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace;use Ada_Lib.Trace;
with AUnit.Options;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Test_Results;
with Camera.Commands.Unit_Test;
with Camera.Lib.Base.Command_Tests;
with Camera.Lib.Base.Test;
with Camera.Lib.Options.Unit_Test;
with Camera.Main;
with Configuration.Camera.Setup.Unit_Tests;
with Configuration.Camera.State.Unit_Tests;
with Widgets.Adjust.Unit_Test;
with Widgets.Control.Unit_Test;

   ---------------------------------------------------------------
procedure Camera.Lib.Unit_Test.Run_Suite (
   Options  : in     Camera.Lib.Options.Unit_Test.
                        Camera_Unit_Test_Program_Options_Type) is
   pragma Unreferenced (Options);
   ---------------------------------------------------------------

   Debug    : Boolean renames Camera.Lib.Options.Unit_Test.
               Camera_Lib_Unit_Test.Unit_Test_Debug;
   Nested_Options
      : constant Ada_Lib.Options.Unit_Test.
            Ada_Lib_Unit_Test_Nested_Options_Constant_Class_Access :=
         Ada_Lib.Options.Unit_Test.
            Get_Readonly_Ada_Lib_Unit_Test_Nested_Options;

begin
--debug := true;
   Log_In (Debug);
   declare
      AUnit_Options  : AUnit.Options.AUnit_Options;
      Outcome        : AUnit.Status;
      Reporter       : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
      Results        : AUnit.Test_Results.Result;
      Test_Suite     : constant AUnit.Test_Suites.Access_Test_Suite :=
                        AUnit.Test_Suites.New_Suite;
   begin
      AUnit_Options.Filter := Nested_Options.Filter'unchecked_access;
--       Test_Suite.Add_Test (Camera.Lib.Base.Test.Suite);
      Test_Suite.Add_Test (Camera.Main.Unit_Test_Suite);
      Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
      Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
      Test_Suite.Add_Test (Standard.Camera.Lib.Base.Test.Suite);
      Test_Suite.Add_Test (Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
      Test_Suite.Add_Test (Standard.Configuration.Camera.State.Unit_Tests.Suite);
      Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);
--       Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
      Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);

      Test_Suite.Run (AUnit_Options, Results, Outcome);
      case Nested_Options.Mode is

         when  Ada_Lib.Options.Driver_Suites |
               Ada_Lib.Options.List_Suites |
               Ada_Lib.Options.Print_Suites =>
            Ada_Lib.Unit_Test.Iterate_Suites (
               Ada_Lib.Options.Unit_Test.Suite_Action'access,
               Ada_Lib.Options.Unit_Test.Routine_Action'access,
               Nested_Options.Mode);

         when Ada_Lib.Options.Run_Tests =>
            Put_Line ("report camera test results");
            Reporter.Report (Results, AUnit_Options);

      end case;
   end;
   Log_Out (Debug or Trace_Options);
end Camera.Lib.Unit_Test.Run_Suite;



