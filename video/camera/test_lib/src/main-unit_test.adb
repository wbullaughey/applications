with Ada_Lib.GNOGA.Unit_Test;
with Ada_Lib.Options;
with Ada_Lib.Strings;
with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with Base;
--with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup; use Configuration.Camera.Setup;
--with Camera.Lib.Unit_Test;
--with Camera.Unit_Test;
with Camera.Lib.Options;
with Events;

package body Main.Unit_Test is

   type Button_Push_Event_Type (
      Description_Pointer  : Ada_Lib.Strings.String_Constant_Access;
      Offset               : Ada_Lib.Timer.Duration_Access
    ) is new Events.Button_Push_Event_Type (
      Description_Pointer  => Description_Pointer,
      Offset               => Offset) with null record;


   Setup_Path                    : constant String := "main_test_setup.cfg";
   State_Path                    : constant String := "main_test_state.cfg";

   ---------------------------------------------------------------
   overriding
   procedure Callback (
      Event                      : in out Button_Push_Event_Type) is
   ---------------------------------------------------------------

--    Mouse_Event                : Gnoga.Gui.Base.Mouse_Event_Record;
--
      Main_Data                  : Main_Data_Type renames Event.Connection_Data.
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

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Options_Type'class
                                    renames Standard.Camera.Lib.Unit_Test.Options.all;
   begin
      Log_In (Debug);
      Test.State.Load_Camera_State (Options.Location, State_Path);
      -- need to load state 1st
      Test.Setup.Load (Test.State, Setup_Path);
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Set_Up;
      Log_Out (Debug);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Camera.Lib.Unit_Test.Unit_Test_Options_Type'class
                                    renames Camera.Lib.Unit_Test.Options.all;
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

   begin
      Log_In (Debug);
      Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (Test).Tear_Down;
      Ada_Lib.GNOGA.Clear_Connection_Data;
      Test.State.Unload;
      Log_Out (Debug);
   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Halt (
      Test                    : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Options                 : Camera.Lib.Unit_Test.Unit_Test_Options_Type'
                                 class renames Camera.Lib.
                                    Unit_Test.Unit_Test_Options_Type'class (
                                       Ada_Lib.Options.Program_Options.all);
      Button_Press_Event      : Button_Push_Event_Type;
      Local_Test              : Test_Type'class renames Test_Type'class (Test);
   begin
      Log_In (Debug);

      if not Options.Test_Driver then
         Log_Here (Debug);
         Button_Press_Event.Connection_Data :=
            Base.Connection_Data_Access (Local_Test.Connection_Data);

         Button_Press_Event.Set (2.0);    -- leave time for web page to display

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
