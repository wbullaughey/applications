with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.AUnit.Ada_Lib_Tests;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Assertions; use AUnit.Assertions;
with Camera.Command_Queue;
with Camera.Commands.Unit_Test;
with Camera.Lib.Base.Command_Tests;
with Camera.Lib.Base.Test;
with Camera.Lib.Connection;
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

   Camera_Lib_Unit_Test_Options  : Camera_Lib_Unit_Test_Options_Class_Access :=
                                    Null;
   Camera_State_Path             : constant String := "camera_state_path.cfg";
   Help_Recursed                 : Boolean := False;
   Initialize_Recursed           : Boolean := False;
   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             Trace_Option & "R",
                                             Ada_Lib.Options.Unmodified);
   Options_Without_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.Options_Type :=
                                          Ada_Lib.Options.Null_Options;
-- Setup_Test_Path               : constant String := "configured_window_setup.cfg";
-- State_Test_Path               : constant String := "configured_window_state.cfg";

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
   function Get_Camera_Lib_Unit_Test_Modifiable_Options (
      From              : in     String := GNAT.Source_Info.Source_Location
   ) return Camera_Lib_Unit_Test_Options_Class_Access is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "Camera_Lib_Unit_Test_Options " &
         (if Camera_Lib_Unit_Test_Options = Null then
            "null "
         else
            "not null ") & From);
      return Camera_Lib_Unit_Test_Options;
   end Get_Camera_Lib_Unit_Test_Modifiable_Options;

   ---------------------------------------------------------------
   function Get_Camera_Lib_Unit_Test_Read_Only_Options (
      From              : in     String := GNAT.Source_Info.Source_Location
   ) return Camera_Lib_Unit_Test_Options_Constant_Class_Access is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      return Camera_Lib_Unit_Test_Options_Constant_Class_Access (
         Camera_Lib_Unit_Test_Options);
   end Get_Camera_Lib_Unit_Test_Read_Only_Options;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Camera_Lib_Unit_Test_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Initialize_Recursed, Debug_Options or Trace_Options,
         "from " & From);
      Camera_Lib_Unit_Test_Options := Options'unchecked_access;

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
      Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out_Checked (Initialize_Recursed,
         Options.Camera_Options.Initialize and then
--       Options.GNOGA_Unit_Test_Options.Initialize and then
--       Options.Initialize and then
         Options.AUnit_Options.Initialize and then
         Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Options_Type (
            Options).Initialize,
         Debug_Options or Trace_Options,  "mode " & Options.Mode'img);
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

-- ----------------------------------------------------------------------------
-- function Options (
--    From                    : in     String :=
--                                        Standard.GNAT.Source_Info.Source_Location
-- ) return Camera_Lib_Unit_Test_Options_Constant_Class_Access is
-- ----------------------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "from " & From);
--    return Protected_Options'access;
-- end Options;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options           : in out Camera_Lib_Unit_Test_Options_Type;
      Iterator          : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option            : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image &
         " options address " & Image (Options'address) &
         " initialized " & Options.Initialized'img &
         " options tag " & Tag_Name (Camera_Lib_Unit_Test_Options_Type'class (Options)'tag));

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
            Options.Camera_Options.Process_Option (Iterator, Option) or else
--          Options.Process_Option (Iterator, Option) or else
            Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Options_Type (
               Options).Process_Option (Iterator, Option),
            Debug_Options or Trace_Options,
            "other " & Option.Image);
      end if;

   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Camera_Lib_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In_Checked (Help_Recursed, Debug_Options or Trace_Options,
         "help mode " & Help_Mode'img);

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
         Put_Line ("      m               main unit test trace, Camera_AUnit");
         Put_Line ("      o               unit_test options");
         Put_Line ("      p               program trace");
         Put_Line ("      s               Configuration.Camera.State unit_test options");
         Put_Line ("      S               Configuration.Camera.Setup unit_test options");
         Put_Line ("      t               unit_test trace");
         Put_Line ("      u               camera commands unit test");
         New_Line;

      end case;

      Options.AUnit_Options.Program_Help (Help_Mode);
      Options.Camera_Options.Program_Help (Help_Mode);
      Ada_Lib.Options.Unit_Test.Ada_Lib_Unit_Test_Options_Type (
         Options).Program_Help (Help_Mode);
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
      Log_In (Debug); --, "options class " & Tag_Name (Options'tag));
not_implemented;
--      if Ada_Lib.Options.Unit_Test.Camera_Lib_Unit_Test_Options_Type (Options).Mode =
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
     Options            : in   Camera_Lib_Unit_Test_Options_Type) is
   ---------------------------------------------------------------

--    Options           : Camera_Lib_Unit_Test_Options_Type'class renames
--                            Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
   begin
      Log_In (Debug, "mode " & Options.Mode'img);

      declare
         AUnit_Options  : AUnit.Options.AUnit_Options;
         Outcome        : AUnit.Status;
         Reporter       : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
         Results        : AUnit.Test_Results.Result;
         Test_Suite     : constant AUnit.Test_Suites.Access_Test_Suite :=
                           AUnit.Test_Suites.New_Suite;

      begin
log_here;
         AUnit_Options.Filter := Options.Filter'unchecked_access;
         Test_Suite.Add_Test (Camera.Lib.Base.Test.Suite);
         Test_Suite.Add_Test (Main.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
         Test_Suite.Add_Test (
            Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
         Test_Suite.Add_Test (
            Standard.Configuration.Camera.State.Unit_Tests.Suite);
         Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);
         Test_Suite.Run (AUnit_Options, Results, Outcome);
log_here;
         case Options.Mode is

            when  Ada_Lib.Options.Driver_Suites |
                  Ada_Lib.Options.List_Suites |
                  Ada_Lib.Options.Print_Suites =>
log_here;
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access,
                  Options.Mode);

            when Ada_Lib.Options.Run_Tests =>
log_here;
               Put_Line ("report camera test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
log_here;
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
   procedure Set_Camera_Queue (
      Brand             : in     Brand_Type;
      Connection_Data   : in out Camera.Lib.Connection.Connection_Data_Type) is
---------------------------------------------------------------

   begin
      case Brand is

         when ALPTOP_Camera =>
            Connection_Data.Camera_Queue := Connection_Data.ALPTOP'unchecked_access;

         when No_Camera =>
            raise Failed with "no camera brand selected";

         when PTZ_Optics_Camera =>
            Connection_Data.Camera_Queue := Connection_Data.PTZ_Optics'unchecked_access;

      end case;
   end Set_Camera_Queue;

---------------------------------------------------------------
  overriding
  procedure Set_Up (
      Test              : in out Camera_Test_Type) is
---------------------------------------------------------------

      Options           : Camera_Lib_Unit_Test_Options_Type'class renames
                              Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Connection_Data   : constant Standard.Camera.Lib.Connection.Connection_Data_Access :=
                           new Camera.Lib.Connection.Connection_Data_Type (
                              Options.Camera_Options.Brand);
      State             : Configuration.Camera.State.State_Type renames
                           Connection_Data.State;
  begin
      Log_In (Debug or Trace_Set_Up, "Set_Up_Load " & Test.Set_Up_Load'img);
      Ada_Lib.GNOGA.Set_Connection_Data (
         Ada_Lib.GNOGA.Connection_Data_Class_Access (Connection_Data));
      Set_Camera_Queue (Options.Camera_Options.Brand, Connection_Data.all);
      Test.Camera_Queue := Connection_Data.Camera_Queue;
      Test.Camera_Address := State.Video_Address;
      Test.Port_Number := State.Video_Port;

      if Test.Set_Up_Load then
         State.Load (Options.Camera_Options.Location, Camera_State_Path);
      end if;
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);

  end Set_Up;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test              : in out Camera_Window_Test_Type) is
   ---------------------------------------------------------------

      Options           : Camera_Lib_Unit_Test_Options_Type'class renames
                              Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Connection_Data   : constant Standard.Camera.Lib.Connection.Connection_Data_Access :=
                           new Camera.Lib.Connection.Connection_Data_Type (
                              Options.Camera_Options.Brand);
--    State             : Configuration.Camera.State.State_Type renames
--                         Connection_Data.State;
   begin
      Log_In (Debug or Trace_Set_Up);
      Set_Camera_Queue (Options.Camera_Options.Brand, Connection_Data.all);
      Connection_Data.Initialize;

      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type(Test).Set_Up;

      if not Test.Initialize_GNOGA then
         Main.Run (
            Directory            => Camera_Lib_Unit_Test_Options.
                                       Camera_Options.Directory.Coerce,
            Port                 => Camera_Lib_Unit_Test_Options.
                                       GNOGA_Options.HTTP_Port,
            Verbose              => True,
            Wait_For_Completion  => False);
      end if;

      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

--   ---------------------------------------------------------------
--   procedure Set_Up_Optional_Load (
--      Test              : in out Camera_Test_Type;
--      Load              : in     Boolean) is
--   ---------------------------------------------------------------
--
--      Options           : Camera_Lib_Unit_Test_Options_Type'class renames
--                              Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
--      Connection_Data   : constant Connection.Connection_Data_Access :=
--                           Connection.Connection_Data_Access (
--                              Ada_Lib.GNOGA.Get_Connection_Data);
--      State             : Configuration.Camera.State.State_Type renames
--                           Connection_Data.State;
--  begin
--      Log_In (Debug or Trace_Set_Up, "load " & Load'img &
--         " brand " & Test.Brand'img &
--         " location " & Test.Location'img);
--
----     if Options.Camera_Options.If_Emulation then
----        Not_Implemented;
----      Emulator.Create;
----      delay 0.2;     -- let emulator initialize
----     end if;
--
--     if Load then
--        State.Load (Options.Camera_Options.Location, Camera_State_Path);
--        Test.Camera_Address := State.Video_Address;
--        Test.Port_Number := State.Video_Port;
--        Test.Camera_Queue := Connection_Data.Camera_Queue;
--
----      case Test.Brand is
----
----         when Standard.Camera.Lib.ALPTOP_Camera =>
----             not_implemented;
----
----         when Standard.Camera.LIB.PTZ_Optics_Camera =>
----             Test.Camera_Queue := Test.PTZ_Optics'unchecked_access;
----             Test.Camera_Queue.Open (State.Video_Address.all, Test.Port_Number);
----
----         when Standard.Camera.Lib.No_Camera =>
----            raise Failed with "no camera set";
----
----      end case;
--     end if;
--     Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
--     Log_Out (Debug or Trace_Set_Up);
--
--  exception
--     when Fault: others =>
--        Test.Set_Up_Exception (Fault);
--   end Set_Up_Optional_Load;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Camera_Test_Type) is
   ---------------------------------------------------------------

      Connection_Data            : Standard.Camera.Lib.Connection.Connection_Data_Type renames
                                    Standard.Camera.Lib.Connection.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug);
      Test.Camera_Queue.Close;
      State.Unload;

      Gnoga.Application.Multi_Connect.End_Application;
      delay 0.2;

      Ada_Lib.GNOGA.Clear_Connection_Data;
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug);
   end Tear_Down;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Camera_Lib_Unit_Test_Options_Type;
      Iterator          : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

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
               Options.Debug := True;
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

            when 's' =>
               Standard.Configuration.Camera.State.Unit_Tests.Debug := True;

            when 'S' =>
               Standard.Configuration.Camera.Setup.Unit_Tests.Debug := True;

            when 't' =>
               Debug := True;

--          when 'w' =>
--             Widget_Trace := True;

            when 'u' =>
               Camera.Commands.Unit_Test.Debug := True;

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
--Trace_Set_Up := True;
-- Include_Program := True;
   Log_Here (Debug or Trace_Options);
-- Options := Protected_Options'access;
end Camera.Lib.Unit_Test;

