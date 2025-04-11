with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Help;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Options;
with AUnit.Test_Cases;
with AUnit.Test_Results;
--with Camera.Lib.Unit_Test;
with Command_Name;
with Main;

package body Driver.Unit_Test is

   procedure Run_All_Tests  (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Run_Routine  (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Run_Suite  (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Debug                         : Boolean := False;
   Debug_Options                 : Boolean := False;
   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Create_Options (
                                          "", -- renived Trace_Option,
                                          Ada_Lib.Options.Unmodified);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Null_Options;
--                                     Ada_Lib.Options.
--                                        Create_Options ("r");
   Protected_Options             : aliased Driver_Unit_Test_Options_Type;

   ---------------------------------------------------------------
   function Create_Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    AUnit.Test_Suites.New_Suite;
      Tests                      : constant Driver_Test_Access :=
                                    new Driver_Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Create_Suite;

   ----------------------------------------------------------------
   function Get_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Unit_Test_Option_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug_Options or Trace_Options, "from " & From);
      return Protected_Options'access;
   end Get_Modifiable_Options;

   ---------------------------------------------------------------
   function Get_Readonly_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Unit_Test_Option_Constant_Class_Access is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug_Options or Trace_Options, "from " & From);
      return Protected_Options'access;
   end Get_Readonly_Options;

   ---------------------------------------------------------------
   function Initialize
   return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options,
         " Initialized " & Protected_Options.Initialized'img);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters, Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);

--    Protected_Options.Unit_Test := True;
      Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);

--    Ada_Lib.Options.Unit_Test.Unit_Test_Options :=
--       Protected_Options'unchecked_access;

      Set_Protected_Options (Protected_Options.Driver_Options'access);
      Protected_Options.Driver_Options.Camera_Directory.Construct (
         "../../unit_test");

      return Log_Out (
         Protected_Options.Driver_Options.Initialize and then
         Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type (
            Protected_Options).Initialize and then
         Protected_Options.Process (
            Include_Options      => True,
            Include_Non_Options  => False,
            Modifiers            => Ada_Lib.Help.Modifiers),
         Debug_Options or Trace_Options,
         "Initialized " & Protected_Options.Initialized'img);

   exception

      when Fault: others =>
         Trace_Exception (True, Fault);
         raise;

   end Initialize;

   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : Driver_Test_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   function Process_Option (  -- process one option
     Options                     : in out Driver_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean is
   ---------------------------------------------------------------

--    Suite                      : Ada_Lib.Strings.Unlimited.String_Type;

   begin
      Log_In (Debug_Options or Trace_Options, Option.Image);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is

--         when 'l' =>
--             Protected_Options.Driver_Options.List_Output := True;

--         when 'r' =>
--             Protected_Options.Driver_Options.Remote_Camera := True;

           when Trace_Option => -- 'T'
              declare
                 Parameter
                          : constant String := Iterator.Get_Parameter;
              begin
                 Log (Debug_Options or Trace_Options, Here, " process parameter  " & Quote (Parameter));

                 for Trace of Parameter loop
                    Log_Here (Debug_Options or Trace_Options, Quote ("Trace", Trace));
                    case Trace is

                       when 'a' =>
                          Debug := True;
                          Debug_Options := True;
                          Main.Debug := True;
                          Protected_Options.Driver_Options.Driver_Debug := True;

                       when 'd' =>
                          Protected_Options.Driver_Options.Driver_Debug := True;

                       when 'm' =>
                          Main.Debug := True;

                       when 'o' =>
                          Debug_Options := True;

                       when 't' =>
                          Debug := True;

                       when others =>
                          Options.Bad_Option (Quote (
                             "unexpected trace option", Trace));
                          return False;

                    end case;
                 end loop;
              end;

              when others =>
                  declare
                     Message  : constant String :=
                                 "Has_Option incorrectly passed " & Option.Image;
                  begin
                     Log_Exception (Trace_Options or Debug_Options, Message);
                     raise Failed with Message;
                  end;

         end case;
      else
         return Log_Out (Options.Driver_Options.Process_Option (
            Iterator, Option) or else
            Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type (
               Options).Process_Option (Iterator, Option),
            Debug_Options or Trace_Options, "other " & Option.Image);
      end if;

      Log_Out (Debug_Options or Trace_Options,
         " option" & Option.Image & " handled");
      return True;
   end Process_Option;

   ---------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Driver_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ---------------------------------------------------------------

      Component                  : constant String := Command_Name;

   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img);
      Options.Driver_Options.Program_Help (Help_Mode);
      Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type (
         Options).Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
--       Ada_Lib.Help.Add_Option ('l', "", "list output from camera app",
--          Component);
--       Ada_Lib.Help.Add_Option ('r', "", "remote camera", Component);
         Ada_Lib.Help.Add_Option (Trace_Option, "trace options",
            "driver unit test trace options", Component);

      when Ada_Lib.Options.Traces =>
         New_Line;
         Put_Line (Command_Name & " trace options (-" & Trace_Option &")");
         Put_Line ("      a               all");
         Put_Line ("      A               AUnit");
         Put_Line ("      m               Main Window");
         Put_Line ("      o               unit_test options");
         Put_Line ("      t               driver test trace");

      end case;
      Log_Out (Debug_Options or Trace_Options);

   end Program_Help;

     ---------------------------------------------------------------
     overriding
     procedure Register_Tests (
        Test                       : in out Driver_Test_Type) is
     ---------------------------------------------------------------

     begin
        Log_In (Debug);

        Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
           Routine        => Run_Routine'access,
           Routine_Name   => AUnit.Format ("Run_Routine")));

        Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
           Routine        => Run_Suite'access,
           Routine_Name   => AUnit.Format ("Run_Suite")));

        Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
           Routine        => Run_All_Tests'access,
           Routine_Name   => AUnit.Format ("Run_All_Tests")));

        Log_Out (Debug);
     end Register_Tests;
--
   ---------------------------------------------------------------
   procedure Run_Suite is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      declare
         AUnit_Options           : AUnit.Options.AUnit_Options;
         Outcome                 : AUnit.Status;
         Reporter                : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
         Results                 : AUnit.Test_Results.Result;
         Test_Suite              : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    AUnit.Test_Suites.New_Suite;

      begin
         AUnit_Options.Filter := Protected_Options.Filter'unchecked_access;
         Test_Suite.Add_Test (Create_Suite);

         Log_Here (Debug, "mode " & Protected_Options.Mode'img);
         case Protected_Options.Mode is

            when Ada_Lib.Options.Driver_Suites |
                 Ada_Lib.Options.List_Suites |
                 Ada_Lib.Options.Print_Suites =>
               Test_Suite.Run (AUnit_Options, Results, Outcome);
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access,
                  Protected_Options.Mode);

            when Ada_Lib.Options.Run_Tests =>
               declare
                  Routine        : constant String :=
                                    Protected_Options.Routine.Coerce;
                  Suite          : constant String :=
                                    Protected_Options.Suite_Name.Coerce;
               begin
                  Log_Here (Debug, Quote ("suite", Suite) &
                     Quote (" routine", Routine));

                  Test_Suite.Run (AUnit_Options, Results, Outcome);

                  if Suite'length > 0 and then
                        not Ada_Lib.Unit_Test.Has_Test (Suite,
                           (if Routine'length > 0 then
                                 Routine
                              else
                                 "")) then
                     raise Failed with Quote ("suite", Suite) &
                        (if Routine'length > 0 then
                              Quote (", routine", Routine)
                           else
                              "") &
                        " does not exist";
                  end if;
               end;
               Put_Line ("report camera test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
      end;
      Log_Out (Debug);
   end Run_Suite;

     ---------------------------------------------------------------
     procedure Run_All_Tests  (
        Test                       : in out AUnit.Test_Cases.Test_Case'class) is
     pragma Unused (Test);
     ---------------------------------------------------------------

     begin
        Log_In (Debug);
        Protected_Options.Routine.Construct ("");
        Protected_Options.Routine.Construct ("");
        Queue_Tests;
        Run_Selection;
        Log_Out (Debug);
     end Run_All_Tests;

     ---------------------------------------------------------------
     procedure Run_Routine  (
        Test                       : in out AUnit.Test_Cases.Test_Case'class) is
     pragma Unused (Test);
     ---------------------------------------------------------------

     begin
        Log_In (Debug);
        Protected_Options.Suite_Name.Construct ("Main");
        Protected_Options.Routine.Construct ("Test_Halt");
        Queue_Tests;
        Run_Selection;
        Log_Out (Debug);

     exception

         when Fault: others =>
            Ada_Lib.Unit_Test.Exception_Assert (Fault);

     end Run_Routine;

     ---------------------------------------------------------------
     procedure Run_Suite  (
        Test                       : in out AUnit.Test_Cases.Test_Case'class) is
     pragma Unused (Test);
     ---------------------------------------------------------------

     begin
        Log_In (Debug);
        Protected_Options.Suite_Name.Construct ("Main");
        Protected_Options.Routine.Construct ("");
        Queue_Tests;
        Run_Selection;
        Log_Out (Debug);
     end Run_Suite;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug_Options or Trace_Options or Elaborate);
end Driver.Unit_Test;
