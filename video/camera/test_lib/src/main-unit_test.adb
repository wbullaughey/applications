with Ada_Lib.GNOGA.Unit_Test;
with Ada_Lib.Options.Actual;
with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Test_Cases;
with Base;
with Camera.Lib.Options;
with Camera.Lib.Unit_Test;
--with Configuration.Camera.Setup; use Configuration.Camera.Setup;
with Configuration.Camera.State;
with GNOGA_Ada_Lib;

package body Main.Unit_Test is

   type Test_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type) is new
                                    Camera.Lib.Unit_Test.
                                       With_Camera_With_GNOGA_Test_Type (
                                          Brand             => Brand,
                                          Initialize_GNOGA  => True) with
                                             null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

-- overriding
-- procedure Set_Up (
--    Test                       : in out Test_Type
-- ) with Pre => not Test.Verify_Set_Up,
--        Post => Test.Verify_Set_Up;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type);

   procedure Test_Halt (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Preset_Library (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   type Button_Push_Event_Type   is new Ada_Lib.Timer.Event_Type
                                    with null record;

   overriding
   procedure Callback (
      Event                      : in out Button_Push_Event_Type);

-- Setup_Path                    : constant String := "main_test_setup.cfg";
-- State_Path                    : constant String := "main_test_state.cfg";
   Suite_Name                    : constant String := "Main";


   ---------------------------------------------------------------
   overriding
   procedure Callback (
      Event                      : in out Button_Push_Event_Type) is
   pragma Unreferenced (Event);
   ---------------------------------------------------------------

      Main_Data                  : Main_Data_Type renames
                                    Base.Connection_Data_Type (
                                       GNOGA_Ada_Lib.Get_Connection_Data.all).
                                          Main_Data.all;
      View                       : View_Type renames Main_Data.View;
      Docker                     : Docker_Type renames View.Docker;
      Panel                      : Panel_Type renames Docker.Panel;
      Navigation                 : Navigation_Type renames Panel.Navigation;
      Exit_Button                : Gnoga.Gui.Element.Common.Button_Type renames
                                    Navigation.Exit_Button;
   begin
      Log_In (Debug);
      Pause_On_Flag ("before exit button", Here, Debug);
      Exit_Button.Fire_On_Click;
      Log_Out (Debug);
   end Callback;

   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Test_Type
   ) return Standard.AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return Standard.AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Halt'access,
         Routine_Name   => AUnit.Format ("Test_Halt")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Preset_Library'access,
--       Routine_Name   => AUnit.Format ("Test_Preset_Library")));
      Log_Out (Debug);

   end Register_Tests;

-- ---------------------------------------------------------------
-- overriding
-- procedure Set_Up (
--    Test                       : in out Test_Type) is
-- ---------------------------------------------------------------
--
--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
--
-- begin
--    Log_In (Debug or Trace_Set_Up);
--    Camera.Lib.Unit_Test.No_Camera_With_GNOGA_Test_Type (Test).Set_Up;
--       -- allocate connection data
--    declare
--       Connection_Data         : Base.Connection_Data_Type renames
--                                  Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
--       State                   : Configuration.Camera.State.State_Type renames
--                                  Connection_Data.State;
--    begin
--       State.Load (Options.Camera_Options.Location, State_Path);
--       -- need to load state 1st
--       Test.Setup.Load (State, Setup_Path);
--    end;
--    Log_Out (Debug or Trace_Set_Up);
-- end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'class
                                    renames Camera.Lib.Unit_Test.
                                       Get_Camera_Unit_Test_Constant_Options.all;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type (
                                    Options.Camera_Options.Brand);

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

      State                      : Configuration.Camera.State.State_Type renames
                                    Test.State;
   begin
      Log_In (Debug or Trace_Set_Up);
      Camera.Lib.Unit_Test.With_Camera_With_GNOGA_Test_Type (Test).Tear_Down;
--    GNOGA_Ada_Lib.Clear_Connection_Data;
      State.Unload;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Halt (
      Test                    : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

      Options                 : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'
                                 class renames Camera.Lib.
                                    Unit_Test.Unit_Test_Options_Constant_Class_Access (
                                       Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).all;
      Button_Press_Event      : Button_Push_Event_Type;
--    Local_Test              : Test_Type'class renames Test_Type'class (Test);
   begin
      Log_In (Debug, "Test_Driver " & Options.Test_Driver'img);

      if not Options.Test_Driver then
         Log_Here (Debug);
--       Button_Press_Event.Connection_Data :=
--          Base.Connection_Data_Access (Local_Test.Connection_Data);

         Button_Press_Event.Start (2.0, "halt wait");
            -- leave time for web page to display

         Run (
            Directory            => Camera.Lib.Options.Current_Directory,
            Port                 => Options.GNOGA_Options.HTTP_Port,
            Verbose              => True,
            Wait_For_Completion  => True);
         end if;
      Log_Out (Debug);

   exception
      when Fault : others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

   end Test_Halt;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end Main.Unit_Test;
