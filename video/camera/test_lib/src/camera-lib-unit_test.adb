with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.AUnit.Ada_Lib_Tests;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Assertions; use AUnit.Assertions;
with Base;
with Camera.Command_Queue;
with Camera.Commands.Unit_Test;
with Camera.Lib.Base.Command_Tests;
with Camera.Lib.Options;
with Configuration.Camera.Setup.Unit_Tests;
with Configuration.Camera.State.Unit_Tests;
with Gnoga.Application.Multi_Connect;
with Main.Unit_Test;
--with Runtime_Options;
with Widgets.Adjust.Unit_Test;
with Widgets.Control.Unit_Test;
with Widgets.Configured.Unit_Test;

package body Camera.Lib.Unit_Test is

   use type Ada_Lib.Options.Mode_Type;

   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             Trace_Option & "R");
   Options_Without_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type :=
                                          Ada_Lib.Options.Null_Options;
   Help_Recursed                 : Boolean := False;
   Initialize_Recursed           : Boolean := False;
   Protected_Options             : aliased Unit_Test_Program_Options_Type;

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   Camera_State_Path             : constant String := "camera_state_path.cfg";
   Setup_Test_Path               : constant String := "configured_window_setup.cfg";
   State_Test_Path               : constant String := "configured_window_state.cfg";

   ----------------------------------------------------------------------------
   procedure Add_Test (
      Suite                      : access Test_Suite'Class;
      Test                       : access AUnit.Simple_Test_Cases.Test_Case'Class) is
   pragma Unreferenced (Suite, Test);
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug);
   end Add_Test;

   ---------------------------------------------------------------
   function Initialize
   return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options);
      Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);

--    Ada_Lib.Options.Unit_Test.Unit_Test_Options :=
--       Ada_Lib.Options.Unit_Test.Unit_Test_Options_Constant_Class_Access'(
--          Protected_Options'unchecked_access);

      return Log_Out (
         Protected_Options.Initialize and then
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

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Unit_Test_Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Initialize_Recursed, Debug_Options or Trace_Options,
         "from " & From);
      Unit_Test_Options := Options'unchecked_access;
      Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
      Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out_Checked (Initialize_Recursed,
--       Options.Camera_Options.Initialize and then
         Options.AUnit_Options.Initialize and then
--       Options.GNOGA_Unit_Test_Options.Initialize and then
         Options.Unit_Test.Initialize and then
--       Ada_Lib.Options.AUnit.Ada_Lib_Tests.Initialize and then
         Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

-- ---------------------------------------------------------------
-- overriding
-- function Name (
--    Test                       : in     Camera_Test_Type
-- ) return AUnit.Message_String is
-- ---------------------------------------------------------------
--
-- begin
--    return AUnit.Format (Suite_Name);
-- end Name;

-- ----------------------------------------------------------------------------
-- function Options (
--    From                    : in     String :=
--                                        Standard.GNAT.Source_Info.Source_Location
-- ) return Options_Constant_Class_Access is
-- ----------------------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "from " & From);
--    return Protected_Options.Camera_Options'access;
-- end Options;

   ----------------------------------------------------------------------------
   function Options (
      From                    : in     String :=
                                          Standard.GNAT.Source_Info.Source_Location
   ) return Unit_Test_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      return Protected_Options'access;
   end Options;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Unit_Test_Program_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image &
         " options address " & Image (Options'address) &
         " initialized " & Options.Initialized'img &
         " options tag " & Tag_Name (Unit_Test_Program_Options_Type'class (Options)'tag));

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is

            when Trace_Option =>    -- T
               Options.Trace_Parse (Iterator);

            when others =>
               raise Failed with "Has_Option incorrectly passed" & Option.Image;

         end case;

         return Log_Out (True, Debug_Options or Trace_Options,
            " option" & Option.Image & " handled");
      else
         return Log_Out (
            Options.AUnit_Options.Process_Option (Iterator, Option) or else
            Options.Unit_Test.Process_Option (Iterator, Option) or else
            Options_Type (Options).Process_Option (Iterator, Option),
            Debug_Options or Trace_Options,
            "other " & Option.Image);
      end if;

   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In_Checked (Help_Recursed, Debug_Options or Trace_Options,
         "help mode " & Help_Mode'img);
      Options.AUnit_Options.Program_Help (Help_Mode);
      Options.Unit_Test.Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option ('T', "trace options",
            "Camera Lib unit test", "Camera.Lib.Unit_Test");
         New_Line;

      when Ada_Lib.Options.Traces =>
         Put_Line ("Camera Lib Unit Test trace options (" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      A               Widgets.Adjust unit test trace");
         Put_Line ("      b               Base Command_Tests");
         Put_Line ("      c               Widgets.Control unit_test trace");
         Put_Line ("      C               Widgets.Configured unit test trace");
         Put_Line ("      d               library unit test trace");
         Put_Line ("      m               main unit test trace");
         Put_Line ("      o               unit_test options");
         Put_Line ("      p               program trace");
         Put_Line ("      q               camera queue");
         Put_Line ("      s               Configuration.Camera.State unit_test options");
         Put_Line ("      S               Configuration.Camera.Setup unit_test options");
         Put_Line ("      t               unit_test trace");
         New_Line;

      end case;

     Options_Type (Options).Program_Help (Help_Mode);
     Log_Out_Checked (Help_Recursed, Debug_Options or Trace_Options);

   end Program_Help;

-- ------------------------------------------------------------
-- procedure Routine_Action (
--    Suite                      : in     String;
--    Routine                    : in     String;
--    Mode                       : in     Ada_Lib.Options.Mode_Type) is
-- ------------------------------------------------------------
--
-- begin
--    Log_In (Debug, Quote ("Suite", Suite) & Quote (" Routine", Routine) &
--       " Suites_Mode " & Mode'img);
--
--    case Mode is
--
--       when Ada_Lib.Options.List_Suites =>
--          Put_Line ("suite:" & Suite & " routine:" & Routine);
--
--       when Ada_Lib.Options.Print_Suites =>
--          Put_Line ("      " & Routine);
--
--       when Ada_Lib.Options.Run_Tests =>
--          null;
--
--    end case;
-- end Routine_Action;

   ---------------------------------------------------------------
   overriding
   procedure Run (
      Suite                      : access Test_Suite;
      Options                    :        AUnit.Options.AUnit_Options;
      Results                    : in out AUnit.Test_Results.Result'Class;
      Outcome                    :    out AUnit.Status) is
   pragma Unreferenced (Outcome);
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "options class " & Tag_Name (Camera_Options'tag));
not_implemented;
--      if Ada_Lib.Options.Unit_Test.Unit_Test_Program_Options_Type (Options).Mode =
--            Ada_Lib.Options.Run_Tests then
--         Log_Here (Debug);
----       Register_Tests (Test_Case'Class (Test.all));
--      else
--         AUnit.Test_Suites.Test_Suite (Suite.all).Run (Options, Results, OutCome);
--      end if;
      Log_Out (Debug);
   end Run;

   ---------------------------------------------------------------
   procedure Run_Suite (
     Options                    : in   Unit_Test_Program_Options_Type) is
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
         AUnit_Options.Filter := Options.Unit_Test.Filter'unchecked_access;
--       Test_Suite.Add_Test (Camera.Lib.Base.Test.Suite);
         Test_Suite.Add_Test (Main.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
         Test_Suite.Add_Test (Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
         Test_Suite.Add_Test (Standard.Configuration.Camera.State.Unit_Tests.Suite);
         Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);

         Test_Suite.Run (AUnit_Options, Results, Outcome);
         case Options.Unit_Test.Mode is

            when  Ada_Lib.Options.Driver_Suites |
                  Ada_Lib.Options.List_Suites |
                  Ada_Lib.Options.Print_Suites =>
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access,
                  Options.Unit_Test.Mode);

            when Ada_Lib.Options.Run_Tests =>
               Put_Line ("report camera test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
      end;
      Log_Out (Debug or Trace_Options);
   end Run_Suite;

-- ---------------------------------------------------------------
-- procedure Run_Suite (
--    Test                       : in     Camera_Window_Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "List suties " & Options.Mode'img);
--    declare
--       AUnit_Options           : AUnit.Options.AUnit_Options;
--       Outcome                 : AUnit.Status;
--       Reporter                : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
--       Results                 : AUnit.Test_Results.Result;
--       Test_Suite              : constant AUnit.Test_Suites.Access_Test_Suite :=
--                                  AUnit.Test_Suites.New_Suite;
--
--    begin
--       AUnit_Options.Unit_Test.Filter := Options.Filter'unchecked_access;
--       Test_Suite.Add_Test (Standard.Configuration.Camera.State.Unit_Tests.Suite);
--       Test_Suite.Add_Test (Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
--       Test_Suite.Add_Test (Main.Unit_Test.Suite);
--       Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
--       Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
--       Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);
--       Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
--       Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);
--
--       Log_Here (Debug);
--       Test_Suite.Run (AUnit_Options, Results, Outcome);
--       case Options.Mode is
--
--          when Ada_Lib.Options.Driver_Suites |
--               Ada_Lib.Options.List_Suites |
--               Ada_Lib.Options.Print_Suites =>
--             Ada_Lib.Unit_Test.Iterate_Suites (
--                Ada_Lib.Options.Unit_Test.Suite_Action'access,
--                Ada_Lib.Options.Unit_Test.Routine_Action'access, Options.Mode);
--
--          when Ada_Lib.Options.Run_Tests =>
--             Put_Line ("report camera test results");
--             Reporter.Report (Results, AUnit_Options);
--
--       end case;
--    end;
--    Log_Out (Debug);
-- end Run_Suite;

---------------------------------------------------------------
  overriding
  procedure Set_Up (
      Test                       : in out Camera_Test_Type) is
---------------------------------------------------------------

  begin
     Test.Set_Up_Optional_Load (True);
  end Set_Up;

   ---------------------------------------------------------------
   procedure Set_Up_Optional_Load (
      Test                       : in out Camera_Test_Type;
      Load                       : in     Boolean) is
   ---------------------------------------------------------------

      Connection_Data            : constant Standard.Base.Connection_Data_Access :=
                                    new Standard.Base.Connection_Data_Type;
      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Options.all;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
  begin
      Log_In (Debug or Trace_Set_Up, "load " & Load'img &
         " brand " & Test.Brand'img &
         " location " & Test.Location'img);
      Ada_Lib.GNOGA.Set_Connection_Data (
         Ada_Lib.GNOGA.Connection_Data_Class_Access (Connection_Data));

--     if Options.If_Emulation then
--        Not_Implemented;
--      Emulator.Create;
--      delay 0.2;     -- let emulator initialize
--     end if;

      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;

     if Load then
        State.Load (Options.Camera_Options.Location, Camera_State_Path);
        Test.Camera_Address := State.Video_Address;
        Test.Port_Number := State.Video_Port;

        case Test.Brand is

           when Standard.Camera.Lib.ALPTOP_Camera =>
               not_implemented;

           when Standard.Camera.LIB.PTZ_Optics_Camera =>
               Test.Camera := Test.PTZ_Optics'unchecked_access;
               Test.Camera.Open (State.Video_Address.all, Test.Port_Number);

           when Standard.Camera.Lib.No_Camera =>
              raise Failed with "no camera set";

        end case;
     end if;
     Log_Out (Debug or Trace_Set_Up);

  exception
     when Fault: others =>
        Test.Set_Up_Exception (Fault);
   end Set_Up_Optional_Load;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                    : in out Camera_Window_Test_Type) is
   ---------------------------------------------------------------

      Connection_Data         : constant Standard.Base.Connection_Data_Access :=
                                 new Standard.Base.Connection_Data_Type;
      Options                 : Standard.Camera.Lib.Unit_Test.
                                 Unit_Test_Program_Options_Type'class
                                    renames Standard.Camera.Lib.Unit_Test.
                                       Options.all;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug or Trace_Set_Up);
      Ada_Lib.GNOGA.Set_Connection_Data (
         Ada_Lib.GNOGA.Connection_Data_Class_Access (Connection_Data));
      Connection_Data.Initialize;
      State.Load (
         Options.Camera_Options.Location, State_Test_Path); -- need to load state 1st
      Test.Setup.Load (State, Setup_Test_Path);
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type(Test).Set_Up;

         if not Test.Initialize_GNOGA then
            Main.Run (
               Directory            => Camera.Lib.Options.Current_Directory,
               Port                 => Options.GNOGA_Options.HTTP_Port,
               Verbose              => True,
               Wait_For_Completion  => False);
         end if;

      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

-- ------------------------------------------------------------
-- procedure Suite_Action (
--    Suite                      : in     String;
--    First                      : in out Boolean;
--    Mode                       : in     Ada_Lib.Options.Mode_Type) is
-- ------------------------------------------------------------
--
-- begin
--    Log_In (Debug, Quote ("Suite", Suite) & " first " & First'img &
--       " Mode " & Mode'img);
--
--    case Mode is
--
--       when Ada_Lib.Options.List_Suites |
--            Ada_Lib.Options.Run_Tests =>
--          null;
--
--       when Ada_Lib.Options.Print_Suites =>
--          if First then
--             Put_Line ("test suites: ");
--             First := False;
--          end if;
--          Put_Line ("   " & Suite);
--
--    end case;
--    Log_Out (Debug);
-- end Suite_Action;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Camera_Test_Type) is
   ---------------------------------------------------------------

      Connection_Data            : Standard.Base.Connection_Data_Type renames
                                    Standard.Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug);
      Test.Camera.Close;
      State.Unload;

      Gnoga.Application.Multi_Connect.End_Application;
      delay 0.2;

      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug);
   end Tear_Down;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Unit_Test_Program_Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;
--    Widget_Trace               : Boolean := False;

   begin
      Log (Debug_Options or Trace_Options, Here, " process parameter  " & Quote (Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug_Options, Quote ("Trace", Trace));
         case Trace is

            when 'a' =>
               Camera.Command_Queue.Debug := True;
               Standard.Camera.Lib.Base.Command_Tests.Debug := True;
               Standard.Configuration.Camera.Setup.Unit_Tests.Debug := True;
               Standard.Configuration.Camera.State.Unit_Tests.Debug := True;
               Debug := True;
               Debug_Options := True;
               Main.Unit_Test.Debug := True;
               Options.Main_Debug := True;
               Options.Unit_Test.Debug := True;
               Widgets.Adjust.Unit_Test.Debug := True;
               Widgets.Control.Unit_Test.Debug := True;
               Widgets.Configured.Unit_Test.Debug := True;

            when 'A' =>
               Widgets.Adjust.Unit_Test.Debug := True;

            when 'b' =>
               Standard.Camera.Lib.Base.Command_Tests.Debug := True;

            when 'c' =>
               Widgets.Control.Unit_Test.Debug := True;

            when 'C' =>
               Widgets.Configured.Unit_Test.Debug := True;

            when 'd' =>
               Debug := True;

            when 'm' =>    -- Main unit tests
               Main.Unit_Test.Debug := True;

            when 'o' =>    -- options
               Debug_Options := True;

            when 'p' =>    -- program trace
               Options.Main_Debug := True;

            when 'q' =>    -- camera queue
               Camera.Command_Queue.Debug := True;

            when 's' =>
               Standard.Configuration.Camera.State.Unit_Tests.Debug := True;

            when 'S' =>
               Standard.Configuration.Camera.Setup.Unit_Tests.Debug := True;

            when 't' =>
               Debug := True;

--          when 'w' =>
--             Widget_Trace := True;

            when others =>
               Options.Bad_Option (Quote ("unexpected Camera_Library test trace option",
                  Trace));

         end case;
      end loop;
   end Trace_Parse;
begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
--Debug_Options := True;
--Elaborate := True;
--Trace_Options := True;
-- Include_Program := True;
   Log_Here (Debug or Trace_Options);
-- Options := Protected_Options'access;
end Camera.Lib.Unit_Test;

