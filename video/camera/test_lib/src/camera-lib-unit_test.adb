with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.AUnit.Ada_Lib_Tests;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Assertions; use AUnit.Assertions;
with Base;
with Camera.Command_Queue;
--with Camera.Commands.PTZ_Optics;
with Camera.Commands.Unit_Test;
with Camera.LIB.ALPTOP;
with Camera.Lib.Base.Command_Tests;
with Camera.Lib.Base.Test;
with Camera.Lib.Options;
with Configuration.Camera.Setup.Unit_Tests;
with Configuration.Camera.State.Unit_Tests;
with Gnoga.Application.Multi_Connect;
with Gnoga_Ada_Lib;
--with Hex_IO;
with Main.Unit_Test;
--with Runtime_Options;
with Video.Lib;
with Widgets.Adjust.Unit_Test;
with Widgets.Control.Unit_Test;
with Widgets.Configured.Unit_Test;

package body Camera.Lib.Unit_Test is

   use type Ada_Lib.Options.Mode_Type;
   use type Camera.Address_Constant_Access;
-- use type Camera.Commands.Camera_Class_Access;

   Camera_Description            : aliased constant String := "test camera";
   Trace_Modifier                : constant Character := '@';
   Trace_Option                  : constant Character := '1';
   Options_With_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type :=
                                          Ada_Lib.Options.Create_Options (
                                             Trace_Option, -- & "R",
                                             Ada_Lib.Options.Unmodified);
   Options_Without_Parameters       : aliased constant
                                    Standard.Ada_Lib.Options.
                                       Options_Type :=
                                          Ada_Lib.Options.Null_Options;
   Help_Recursed                 : Boolean := False;
   Initialize_Recursed           : Boolean := False;
-- Protected_Options             : aliased Unit_Test_Program_Options_Type;

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   Camera_State_Path             : constant String := "camera_state_path.cfg";
-- Setup_Test_Path               : constant String := "configured_window_setup.cfg";
-- State_Test_Path               : constant String := "configured_window_state.cfg";
   Test_Setup                    : constant String :=
                                    "test_setup.cfg";

   ----------------------------------------------------------------------------
   procedure Add_Test (
      Suite                      : access Test_Suite'Class;
      Test                       : access AUnit.Simple_Test_Cases.Test_Case'Class) is
   pragma Unreferenced (Suite, Test);
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug);
   end Add_Test;

   ----------------------------------------------------------------------------
   procedure Check_Preset (
      Test                       : in     With_Camera_No_GNOGA_Test_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "set preset");
      Test.Camera_Info.Camera.Set_Preset (Video.Lib.Get_Default_Preset_ID);
      Log_Out (Debug);

   exception
      when Fault: others =>
         Log_Exception (Debug, Fault, "check preset");
         raise;

   end Check_Preset;

   ----------------------------------------------------------------------------
   procedure Dump (
      Test                       : in     With_Camera_No_GNOGA_Test_Type;
      Trace                      : in     Boolean) is
   ----------------------------------------------------------------------------

--    use Ada.Text_IO;

   begin
      if Trace then
         Put_Line ("brand " & Test.Brand'img);
         Put_Line ("Initialize_GNOGA " &
            " Load_State " & Test.Load_State'img &
            " Location " & Test.Camera_Info.Location'img &
            " Port_Number " & Test.Camera_Info.Port_Number'img &
            Quote ("Setup_Path", Test.Setup_Path) &
            Quote ("State_Path", Test.State_Path));
      end if;
   end Dump;

   ----------------------------------------------------------------------------
   function Have_Camera (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Log_Here (Test.Camera_Info.Camera /= Null, Debug or else
         Trace_Pre_Post_Conditions);
   end Have_Camera;

   ----------------------------------------------------------------------------
   function Have_Camera_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Log_Here (Test.Camera_Info.Camera_Address /= Null, Debug);
   end Have_Camera_Address;

   ----------------------------------------------------------------------------
   function Have_Video_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

--    Connection_Data            : constant Standard.Base.Connection_Data_Access :=
--                                  Standard.Base.Connection_Data_Access (
--                                     Gnoga_Ada_Lib.Get_Connection_Data);
      State                      : Configuration.Camera.State.State_Type renames
                                    Test.State;
   begin
      return Log_Here (State.Have_Video_Address, Debug);
   end Have_Video_Address;

   ----------------------------------------------------------------------------
   function Get_Camera_Unit_Test_Constant_Options (
      From                    : in     String := Standard.GNAT.Source_Info.
                                          Source_Location
   ) return Unit_Test_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      return Unit_Test_Options_Constant_Class_Access (
         Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options);
   end Get_Camera_Unit_Test_Constant_Options;

   ----------------------------------------------------------------------------
   procedure Initialize_Camera_Info (
      Camera_Info          : in out Camera_Info_Type) is
   ----------------------------------------------------------------------------


      Options  : Video.Lib.Options_Type'class
                  renames Video.Lib.Options_Constant_Class_Access (
                        Ada_Lib.Options.Actual.
                           Get_Ada_Lib_Read_Only_Nested_Options).all;
   begin
      Camera_Info.Camera_Address := Address_Constant_Access (Options.Camera_Address);
      Camera_Info.Location := Options.Location;
      Camera_Info.Port_Number := Options.Port_Number;
   end Initialize_Camera_Info;

--   ---------------------------------------------------------------
--   function Initialize
--   return Boolean is
--   ---------------------------------------------------------------
--
--   begin
--      Log_In (Debug_Options or Trace_Options);
--log_here;
----    Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);
--
----    Ada_Lib.Options.Unit_Test.Unit_Test_Options :=
----       Ada_Lib.Options.Unit_Test.Unit_Test_Options_Constant_Class_Access'(
----          Protected_Options'unchecked_access);
--
--      return Log_Out (
--         Protected_Options.Initialize and then
--         Protected_Options.Process (
--            Include_Options      => True,
--            Include_Non_Options  => False,
--            Modifiers            => Ada_Lib.Help.Modifiers),
--         Debug_Options or Trace_Options,
--         "Initialized " & Protected_Options.Initialized'img);
--
--   exception
--
--      when Fault: others =>
--         Trace_Exception (True, Fault);
--         raise;
--
--   end Initialize;

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
--    Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
      Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out_Checked (Initialize_Recursed,
         Options.Camera_Options.Initialize and then
--       Options.AUnit_Options.Initialize and then
--       Options.GNOGA_Unit_Test_Options.Initialize and then
--       Options.Unit_Test.Initialize and then
--       Ada_Lib.Options.AUnit.Ada_Lib_Tests.Initialize and then
         Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

---------------------------------------------------------------
   procedure Load_Test_State (
      Camera_Info       : in out Camera_Info_Type;
      Setup             : in out Configuration.Camera.Setup.Setup_Type;
      State             : in out Configuration.Camera.State.State_Type) is
---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
   begin
      Log_In (Debug or Trace_Set_Up);
      State.Load (Options.Camera_Options.Location, Camera_State_Path);
      Log_Here (Debug or Trace_Set_Up, " video port#" & State.Video_Port'img);
      Setup.Load (State, Test_Setup);
      Camera_Info.Camera_Address := State.Video_Address;
      Camera_Info.Port_Number := State.Video_Port;

      Camera_Info.Camera.Initialize_Standard_Preset_IDs;
      Log_Out (Debug or Trace_Set_Up);
   end Load_Test_State;

-- ---------------------------------------------------------------
-- overriding
-- function Name (
--    Test                       : in     With_Camera_No_GNOGA_Test_Type
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
-- ) return Unit_Test_Options_Constant_Class_Access is
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

            when Trace_Option =>    -- 1
               Options.Trace_Parse (Iterator);

            when others =>
               declare
                  Message  : constant String :=
                              "Has_Option incorrectly passed " & Option.Image;
               begin
                  Log_Exception (Trace_Options or Debug_Options, Message);
                  raise Failed with Message;
               end;

         end case;

         return Log_Out (True, Debug_Options or Trace_Options,
            " option" & Option.Image & " handled");
      else
         return Log_Out (
            Options.Camera_Options.Process_Option (Iterator, Option) or else
            Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (Options).Process_Option (Iterator, Option),
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
      Options.Camera_Options.Program_Help (Help_Mode);
--    Options.Unit_Test.Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option (Trace_Option, "trace options",
            "Camera Lib unit test", "Camera.Lib.Unit_Test");
         New_Line;

      when Ada_Lib.Options.Traces =>
         Put_Line ("Camera Lib Unit Test trace options (" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      A               Widgets.Adjust unit test trace");
         Put_Line ("      b               Base Command_Tests");
         Put_Line ("      B               Base Test");
         Put_Line ("      c               Widgets.Control unit_test trace");
         Put_Line ("      C               Widgets.Configured unit test trace");
         Put_Line ("      d               Camera.Lib.Unit_Test.Debug");
         Put_Line ("      m               main unit test trace");
         Put_Line ("      o               unit_test options");
         Put_Line ("      p               program trace");
         Put_Line ("      q               camera queue");
         Put_Line ("      s               Configuration.Camera.State unit_test options");
         Put_Line ("      S               Configuration.Camera.Setup unit_test options");
         Put_Line ("      t               unit_test trace");
         Put_Line ("      @c              Camera Command trace");
         New_Line;

      end case;

     Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (Options).
         Program_Help (Help_Mode);
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
      Log_In (Debug); -- , "options class " & Tag_Name (
--       AUnit.Options.AUnit_Options'class (Options)'tag));
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
         AUnit_Options.Filter := Options.Filter'unchecked_access;
--       Test_Suite.Add_Test (Camera.Lib.Base.Test.Suite);
         Test_Suite.Add_Test (Main.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
         Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
         Test_Suite.Add_Test (Standard.Camera.Lib.Base.Test.Suite);
         Test_Suite.Add_Test (Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
         Test_Suite.Add_Test (Standard.Configuration.Camera.State.Unit_Tests.Suite);
         Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);

         Test_Suite.Run (AUnit_Options, Results, Outcome);
         case Options.Mode is

            when  Ada_Lib.Options.Driver_Suites |
                  Ada_Lib.Options.List_Suites |
                  Ada_Lib.Options.Print_Suites =>
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access,
                  Options.Mode);

            when Ada_Lib.Options.Run_Tests =>
               Put_Line ("report camera test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
      end;
      Log_Out (Debug or Trace_Options);
   end Run_Suite;

-- ---------------------------------------------------------------
-- procedure Run_Suite (
--    Test                       : in     No_Camera_With_GNOGA_Test_Type) is
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
procedure Setup_Camera (
      Load_State     : in     Boolean;
      Brand          : in     Standard.Camera.Lib.Brand_Type;
      Camera_Info    : in out Camera_Info_Type;
      Setup          : in out Configuration.Camera.Setup.Setup_Type;
      State          : in out Configuration.Camera.State.State_Type) is
---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up, "load state " & Load_State'img &
         " location " & Camera_Info.Location'img);

      case Brand is

         when ALPTOP_Camera =>
            Camera_Info.Camera := new Standard.Camera.LIB.ALPTOP.ALPTOP_Type (
               Camera_Description'access);

         when No_Camera =>
            raise Failed with "no camera brand selected";

         when PTZ_Optics_Camera =>
            Camera_Info.Camera := new Standard.Camera.Commands.PTZ_Optics.
               PTZ_Optics_Type (Camera_Description'access);

      end case;

      if Load_State then
         Load_Test_State (Camera_Info, Setup, State);
      end if;

      if Camera_Info.Open_Camera then
         Camera_Info.Camera.Open (
            Camera_Info.Camera_Address.all, Camera_Info.Port_Number);
      end if;
      Log_Out(Debug);
   end Setup_Camera;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                    : in out Camera_Lib_GNOGA_Test_Type) is
   ---------------------------------------------------------------

--    Connection_Data            : constant Standard.Base.Connection_Data_Access :=
--                                    Standard.Base.Allocate_Connection_Data;
--    pragma Unreferenced (Connection_Data); -- called for side affect

   begin
      Log_In (Debug or Trace_Set_Up, "load state " & Test.Load_State'img);
      Standard.Base.Allocate_Connection_Data;

      Configuration.Camera.State.Set_State (Test.State'unchecked_access);
         Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type(Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

---------------------------------------------------------------
  overriding
  procedure Set_Up (
      Test                       : in out With_Camera_No_GNOGA_Test_Type) is
---------------------------------------------------------------

  begin
      Log_In (Debug or Trace_Set_Up, "load " & Test.Load_State'img &
         " brand " & Test.Brand'img);
      Configuration.Camera.State.Set_State (Test.State'unchecked_access);
      Initialize_Camera_Info (Test.Camera_Info);
      Setup_Camera (True, Test.Brand, Test.Camera_Info, Test.Setup, Test.State);

      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up, " location " & Test.Camera_Info.Location'img);

  exception
     when Fault: others =>
        Test.Set_Up_Exception (Fault);
   end Set_Up;

--   ---------------------------------------------------------------
--   overriding
--   procedure Set_Up (
--      Test                    : in out No_Camera_With_GNOGA_Test_Type) is
--   ---------------------------------------------------------------
--
--      Connection_Data         : constant Standard.Base.Connection_Data_Access :=
--                                    Standard.Base.Allocate_Connection_Data;
--      Options                 : Standard.Camera.Lib.Unit_Test.
--                                 Unit_Test_Program_Options_Type'class
--                                    renames Standard.Camera.Lib.Unit_Test.
--                                       Get_Camera_Unit_Test_Constant_Options.all;
--      State                      : Configuration.Camera.State.State_Type renames
--                                    Connection_Data.State;
--   begin
--      Log_In (Debug or Trace_Set_Up);
--      GNOGA_Ada_Lib.Set_Connection_Data (
--         GNOGA_Ada_Lib.Connection_Data_Class_Access (Connection_Data));
--      if Test.Load_State then
--         State.Load (Options.Camera_Options.Location,
--            State_Test_Path); -- need to load state 1st
--      end if;
----    Test.Setup.Load (State, Setup_Test_Path);
--      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type(Test).Set_Up;
--
--      if not Test.Initialize_GNOGA then
--         Main.Run (
--            Directory            => Camera.Lib.Options.Current_Directory,
--            Port                 => Options.GNOGA_Options.HTTP_Port,
--            Verbose              => True,
--            Wait_For_Completion  => False);
--      end if;
--
--      Log_Out (Debug or Trace_Set_Up);
--
--   exception
--      when Fault: others =>
--         Trace_Exception (Debug, Fault);
--         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));
--
--   end Set_Up;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_With_GNOGA_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up,
         " brand " & Test.Brand'img &
         " Load_State " & Test.Load_State'img);
--    Test.Load_State := False;
      Initialize_Camera_Info (Test.Camera_Info);
      Setup_Camera (Test.Load_State, Test.Brand, Test.Camera_Info, Test.Setup,
         Test.State);
      Camera_Lib_GNOGA_Test_Type (Test).Set_Up;

      if not Test.Initialize_GNOGA then
         declare
--          Connection_Data         : constant Standard.Base.Connection_Data_Access :=
--                                        Standard.Base.Allocate_Connection_Data;
            Options                 : Standard.Camera.Lib.Unit_Test.
                                       Unit_Test_Program_Options_Type'class
                                          renames Standard.Camera.Lib.Unit_Test.
                                             Get_Camera_Unit_Test_Constant_Options.all;
--          State                      : Configuration.Camera.State.State_Type renames
--                                        Connection_Data.State;
            begin
               Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type(Test).Set_Up;
               Main.Run (
                  Directory            => Camera.Lib.Options.Current_Directory,
                  Port                 => Options.GNOGA_Options.HTTP_Port,
                  Verbose              => True,
                  Wait_For_Completion  => False);
            end;
         end if;

      Log_Out (Debug or Trace_Set_Up, "location " & Test.Camera_Info.Location'img);

  exception
     when Fault: others =>
        Test.Set_Up_Exception (Fault);
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
      Test                       : in out Camera_Lib_GNOGA_Test_Type) is
   ---------------------------------------------------------------

--    Connection_Data            : Standard.Base.Connection_Data_Type renames
--                                  Standard.Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Test.State;
   begin
      Log_In (Debug or Trace_Set_Up);

      if State.Is_Loaded then
         State.Unload;
      end if;

      Gnoga.Application.Multi_Connect.End_Application;
      delay 0.2;

      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Tear_Down;
      Gnoga_Ada_Lib.Clear_Connection_Data;
      Configuration.Camera.State.Clear_State;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out With_Camera_No_GNOGA_Test_Type) is
   ---------------------------------------------------------------

--    Connection_Data            : Standard.Base.Connection_Data_Type renames
--                                  Standard.Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Test.State;
   begin
      Log_In (Debug or Trace_Set_Up);
      if Test.Camera_Info.Camera /= Null then
         Test.Camera_Info.Camera.Close;
         Test.Camera_Info.Camera := Null; -- needs so test can be rerun
      end if;

      if Test.Setup.Is_Loaded then
         Test.Setup.Unload (State, False);
      end if;
      if State.Is_Loaded then
         State.Unload;
      end if;

      Gnoga.Application.Multi_Connect.End_Application;
      delay 0.2;

      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Configuration.Camera.State.Clear_State;
--    Gnoga_Ada_Lib.Clear_Connection_Data;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Unit_Test_Program_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Extended                   : Boolean := False;
      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log (Debug_Options or Trace_Options, Here, " process parameter  " & Quote (Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug, Quote ("trace", Trace) &
            " extended " & Extended'img);
         case Extended is

            when False =>
               case Trace is

                  when 'a' =>
                     Camera.Command_Queue.Debug := True;
                     Camera_Commands_Debug := True;
                     Standard.Camera.Lib.Base.Command_Tests.Debug := True;
                     Standard.Camera.Lib.Base.Test.Debug := True;
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

                  when 'B' =>
                     Standard.Camera.Lib.Base.Test.Debug := True;

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

                  when Trace_Modifier =>
                     Extended := True;

                  when others =>
                     Options.Bad_Option (Quote (
                        "unexpected Camera_Library test trace option", Trace));

               end case;

            when True =>
               case Trace is

                  when 'c' =>
                     Camera_Commands_Debug := True;

                  when others =>
                     Options.Bad_Trace_Option (Trace_Option, Trace, Trace_Modifier);

               end case;
               Extended := False;

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
