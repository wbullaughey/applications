with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
with Ada_Lib.Options.Create;
--with Ada_Lib.Options.Nested;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Test_States;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Reporter;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Options;
with AUnit.Test_Results;
with Camera.Base;
with Camera.Command_Queue;
with Camera.Commands.Unit_Test;
with Camera.Lib.Base.Command_Tests;
with Camera.Lib.Base.Test;
with Camera.Lib.Options.Unit_Test;
with Camera.Main;
with Camera.State;
--with Camera.States;
with Configuration.Camera.Setup.Unit_Tests;
with Configuration.Camera.State.Unit_Tests;
with Gnoga.Application.Multi_Connect;
with Gnoga_Ada_Lib.Base;
with Widgets.Adjust.Unit_Test;
with Widgets.Control.Unit_Test;

package body Camera.Lib.Unit_Test is

   use type Ada_Lib.Options.Mode_Type;

   Camera_Description            : aliased constant String := "test camera";
   Debug                         : Boolean renames Options.Unit_Test.
                                    Camera_Lib_Unit_Test.Debug;
   Trace_Option                  : constant Character := '1';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Flag_List_Type :=
                                          Ada_Lib.Options.Create.Create_One (
                                             Trace_Option, -- & "R",
                                             Ada_Lib.Options.Unmodified_Flag);
   Help_Recursed                 : Boolean := False;
   Initialize_Recursed           : Boolean := False;

   Camera_State_Path             : constant String := "camera_state_path.cfg";
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
         Trace_Pre_Post_Conditions, "camera not set");
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

      State                      : Configuration.Camera.State.State_Type'class
                                    renames Test.State.all;
   begin
      return Log_Here (State.Have_Video_Address, Debug);
   end Have_Video_Address;

   ----------------------------------------------------------------------------
   function Get_Camera_ID (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------------------

   begin
not_implemented;
return Null_Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------------------
   function Get_Camera_Unit_Test_Constant_Options (
      From                    : in     String := Standard.GNAT.Source_Info.
                                          Source_Location
   ) return Unit_Test_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      return Unit_Test_Options_Constant_Class_Access (
         Ada_Lib.Options.Get_Ada_Lib_Read_Only_Program_Options);
   end Get_Camera_Unit_Test_Constant_Options;

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
         Ada_Lib.Options.Null_Flag_List);
      return Log_Out_Checked (Initialize_Recursed,

         Options.Camera_Library_Options.Initialize and then
         Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

---------------------------------------------------------------
   procedure Load_Test_State (
      Camera_Info       : in out Camera_Info_Type;
      Setup             : in out Configuration.Camera.Setup.Setup_Type) is
--    State             : in out Configuration.Camera.State.State_Type) is
---------------------------------------------------------------

      Options              : Standard.Camera.Lib.Unit_Test.
                              Unit_Test_Program_Options_Type'class
                                 renames Standard.Camera.Lib.Unit_Test.
                                    Get_Camera_Unit_Test_Constant_Options.all;
      Camera_State         : Camera.Base.Camera_State_Access :=
                              States.Get_Writeable_Camera_State;
      Global_State         : Camera.State.State_Access :=
                              Camera.States.Get_Writeable_Global_State;
      Configuration_State  : Configuration.Camera.State.State_Access;

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down);
      Configuration_State.Load (Options.Camera_Library_Options.Location, Camera_State_Path);
--    State.Set_Camera_ID (;
      Log_Here (Debug or Trace_Set_Up_Tear_Down,
         " video port#" & Configuration.Camera.State.Video_Port'img);
      Setup.Load (Camera_State.all, Test_Setup);
      Camera_Info.Camera_Address := Configuration_State.Video_Address;
      Camera_Info.Port_Number := Configuration_State.Video_Port;

      Camera_Info.Camera.Initialize_Standard_Preset_IDs;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);
   end Load_Test_State;

   overriding
   function Process_Option (
      Options  : in out Unit_Test_Program_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image &
         " options address " & Image (Options'address) &
         " initialized " & Options.Initialized'img &
         " options tag " & Tag_Name (Unit_Test_Program_Options_Type'class (Options)'tag));

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Ada_Lib.Options.Null_Flag_List) then
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
            Options.Camera_Library_Options.Process_Option (
               Iterator, Option) or else
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
      Options.Camera_Library_Options.Program_Help (Help_Mode);
--    Options.Unit_Test.Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program_Mode =>
         Ada_Lib.Help.Create_Option (Trace_Option, "trace options",
            "Camera Lib unit test", "Camera.Lib.Unit_Test",
            Ada_Lib.Help.Unmodified_Flag);
         New_Line;

      when Ada_Lib.Options.Trace_Mode =>
         Put_Line ("Camera Lib Unit Test trace options -(" &
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

   ---------------------------------------------------------------
   procedure Run_Suite (
      Test                       : in     No_Camera_With_GNOGA_Test_Type) is
   ---------------------------------------------------------------

      Options  : Ada_Lib.Options.Unit_Test.
                  Ada_Lib_Unit_Test_Program_Options_Type renames
                     Ada_Lib.Options.Unit_Test.
                        Ada_Lib_Unit_Test_Program_Options_Type (
                           Ada_Lib.Options.
                              Get_Ada_Lib_Read_Only_Program_Options.all);
   begin
      Log_In (Debug, "List suties " & Options.Mode'img);
      declare
         AUnit_Options           : AUnit.Options.AUnit_Options;
         Outcome                 : AUnit.Status;
         Reporter                : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
         Results                 : AUnit.Test_Results.Result;
         Test_Suite              : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    AUnit.Test_Suites.New_Suite;

      begin
         AUnit_Options.Filter := Options.Filter'unchecked_access;
         Test_Suite.Add_Test (Standard.Configuration.Camera.State.Unit_Tests.Suite);
         Test_Suite.Add_Test (Standard.Configuration.Camera.Setup.Unit_Tests.Suite);
         Test_Suite.Add_Test (Camera.Main.Unit_Test_Suite);
         Test_Suite.Add_Test (Standard.Camera.Lib.Base.Command_Tests.Suite);
         Test_Suite.Add_Test (Standard.Camera.Commands.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Control.Unit_Test.Suite);
--       Test_Suite.Add_Test (Widgets.Configured.Unit_Test.Suite);
         Test_Suite.Add_Test (Widgets.Adjust.Unit_Test.Suite);

         Log_Here (Debug);
         Test_Suite.Run (AUnit_Options, Results, Outcome);
         case Options.Mode is

            when Ada_Lib.Options.Driver_Suites |
                 Ada_Lib.Options.List_Suites |
                 Ada_Lib.Options.Print_Suites =>
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access, Options.Mode);

            when Ada_Lib.Options.Run_Tests =>
               Put_Line ("report camera test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
      end;
      Log_Out (Debug);
   end Run_Suite;

---------------------------------------------------------------
procedure Setup_Camera (
      Load_State     : in     Boolean;
      Brand          : in     Standard.Camera.Brand_Type;
      Camera_Info    : in out Camera_Info_Type;
      Setup          : in out Configuration.Camera.Setup.Setup_Type) is
--    State          : in out Configuration.Camera.State.State_Type) is
---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down, "load state " & Load_State'img &
         " brand " & Brand'img &
         " location " & Camera_Info.Location'img);

      case Brand is

         when ALPTOP_Camera =>
            Not_Implemented;

         when No_Camera =>
            raise Failed with "no camera brand selected";

         when PTZ_Optics_Camera =>
            Camera_Info.Camera := new Standard.Camera.Commands.PTZ_Optics.
               PTZ_Optics_Type (Camera_Description'access);

      end case;

      if Load_State then
         Load_Test_State (Camera_Info, Setup); -- , State);
      else
         pragma Assert (Camera_Info.Camera_Address /= Null,
            "Camera_Address not initialized");
      end if;

      if Camera_Info.Open_Camera then
         Camera_Info.Camera.Open (
            Camera_Info.Camera_Address.all, Camera_Info.Port_Number);
      end if;
      Log_Out(Debug or Trace_Set_Up_Tear_Down);
   end Setup_Camera;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                    : in out Camera_Lib_GNOGA_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down, "load state " & Test.Load_State'img);
      States.Allocate_Connection_Data (Test.State.Get_Camera_ID);
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);

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

      Options  : Unit_Test_Program_Options_Type'class
                  renames Unit_Test_Options_Constant_Class_Access (
                        Ada_Lib.Options.
                           Get_Ada_Lib_Read_Only_Program_Options).all;

  begin
      Log_In (Debug or Trace_Set_Up_Tear_Down, "load " & Test.Load_State'img &
         " brand " & Test.Brand'img);
      if Test.Load_State then
         Setup_Camera (Test.Load_State, Test.Brand, Test.Camera_Info,
            Test.Setup); -- , Allocate_Camera_State (Test.Get_Camera_ID));
      end if;

      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up_Tear_Down,
         " location " & Test.Camera_Info.Location'img);

  exception
     when Fault: others =>
        Test.Set_Up_Exception (Fault);
   end Set_Up;

   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_With_GNOGA_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down,
         " brand " & Test.Brand'img &
         " Load_State " & Test.Load_State'img &
         " Initialize_GNOGA " & Test.Initialize_GNOGA'img);

      if Test.Load_State then
         Setup_Camera (Test.Load_State, Test.Brand, Test.Camera_Info,
            Test.Setup); --, Test.State);
      end if;

      if not Test.Initialize_GNOGA then
         Log_Here (Debug);
         declare
            Options                 : Standard.Camera.Lib.Unit_Test.
                                       Unit_Test_Program_Options_Type'class
                                          renames Standard.Camera.Lib.Unit_Test.
                                             Get_Camera_Unit_Test_Constant_Options.all;
            begin
               GNOGA_Ada_Lib.Base.Run (
                  Handler              => Camera.Main.On_Connect'Unrestricted_Access,
                  Directory            => Camera.Lib.Options.Current_Directory,
                  Port                 => Options.GNOGA_Options.HTTP_Port,
                  Verbose              => True,
                  Wait_For_Completion  => False);
            end;
         end if;

      Camera_Lib_GNOGA_Test_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up_Tear_Down, "location " & Test.Camera_Info.Location'img);

  exception
     when Fault: others =>
        Test.Set_Up_Exception (Fault);
        Log_Out (Debug or Trace_Set_Up_Tear_Down, "location " & Test.Camera_Info.Location'img);
   end Set_Up;

   overriding
   procedure Tear_Down (
      Test                       : in out Camera_Lib_GNOGA_Test_Type) is
   ---------------------------------------------------------------

      State                      : Configuration.Camera.State.State_Type renames
                                    Test.State;
   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down);

      if State.Is_Loaded then
         State.Unload;
      end if;

      Gnoga.Application.Multi_Connect.End_Application;
      delay 0.2;

      Ada_Lib.Test_States.Clear_Window_Connection_Data (Test.Main_Window);
      State.Clear_Global_Camera_State;
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);

   exception

      when Fault: others =>
         Test.Tear_Down_Failed;
         Trace_Exception (Debug or Trace_Set_Up_Tear_Down, Fault);
         Log_Exception (Debug or Trace_Set_Up_Tear_Down);

   end Tear_Down;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out With_Camera_No_GNOGA_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down);
      begin
         if Test.Camera_Info.Camera /= Null then
            Log_Here (Debug);
            Test.Camera_Info.Camera.Close;
            Test.Camera_Info.Camera := Null; -- needs so test can be rerun
         end if;

         if Test.Setup.Is_Loaded then
            Log_Here (Debug);
            Test.Setup.Unload (Test.State.all, False);
         end if;
         if Test.State.Is_Loaded then
            Log_Here (Debug);
            Test.State.Unload;
         end if;

         Log_Here (Debug);
         Gnoga.Application.Multi_Connect.End_Application;
         delay 0.2;
         Log_Here (Debug);

         Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;

      exception

         when Fault: others =>
            Test.Tear_Down_Failed;
            Trace_Exception (Debug or Trace_Set_Up_Tear_Down, Fault);
            Log_Exception (Debug or Trace_Set_Up_Tear_Down);

      end;
      Log_Here (Debug);
      if Test.State.Has_Camera_ID then
         Test.State.Clear_Global_Camera_State;
      end if;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);
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
                     Camera.Lib.Base.Command_Tests.Debug := True;
                     Camera.Lib.Base.Test.Debug := True;
                     Configuration.Camera.Setup.Unit_Tests.Debug := True;
                     Configuration.Camera.State.Unit_Tests.Debug := True;
                     Camera.Lib.Options.Unit_Test.Camera_Lib_Unit_Test.
                        Debug := True;
                     Debug_Options := True;
                     Camera.Lib.Options.Unit_Test.
                        Camera_Main_Unit_Test.Debug := True;
                     Options.Main_Debug := True;
                     Options.Debug := True;
                     Widgets.Adjust.Unit_Test.Debug := True;
                     Widgets.Control.Unit_Test.Debug := True;
--                   Widgets.Configured.Unit_Test.Debug := True;

                  when 'A' =>
                     Widgets.Adjust.Unit_Test.Debug := True;

                  when 'b' =>
                     Camera.Lib.Base.Command_Tests.Debug := True;

                  when 'B' =>
                     Camera.Lib.Base.Test.Debug := True;

                  when 'c' =>
                     Widgets.Control.Unit_Test.Debug := True;

--                when 'C' =>
--                   Widgets.Configured.Unit_Test.Debug := True;

                  when 'd' =>
                     Camera.Lib.Options.Unit_Test.Camera_Lib_Unit_Test.
                        Debug := True;

                  when 'm' =>    -- Main unit tests
                     Camera.Lib.Options.Unit_Test.Camera_Main_Unit_Test.
                        Debug := True;

                  when 'o' =>    -- options
                     Debug_Options := True;

                  when 'p' =>    -- program trace
                     Options.Main_Debug := True;

                  when 'q' =>    -- camera queue
                     Camera.Command_Queue.Debug := True;

                  when 's' =>
                     Configuration.Camera.State.Unit_Tests.Debug := True;

                  when 'S' =>
                     Configuration.Camera.Setup.Unit_Tests.Debug := True;

                  when 't' =>
                     Debug := True;

                  when Ada_Lib.Help.Trace_Modifier =>
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
                     Options.Bad_Trace_Option (Trace_Option, Trace,
                        Ada_Lib.Help.Trace_Modifier);

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
