with Ada.Exceptions;
with Ada_Lib.Configuration;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Camera.Lib.Unit_Test;

package body Widgets.Control.Unit_Test is

-- use type Gnoga.Gui.View.Pointer_To_View_Base_Class;

   type Button_Push_Event_Type   is new Ada_Lib.Timer.Event_Type with null record;

   type Test_Type (
--    Brand                      : Standard.Camera.Brand_Type;
      Initialize_GNOGA           : Boolean) is new Camera.Lib.Unit_Test.
                                    With_Camera_With_GNOGA_Test_Type (
--                                     Brand             => Brand,
                                       Initialize_GNOGA  => False) with
                                       -- Set_Up will use Main.Run to initialize
                                          null record;

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

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type
   ) with post => Test.Verify_Tear_Down;

   procedure Test_Create_Control (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Suite_Name                    : constant String := "Control";

   Setup_Test_Path               : constant String := "control_window_setup.cfg";
   State_Test_Path               : constant String := "control_window_state.cfg";

   ---------------------------------------------------------------
   overriding
   procedure Callback (
      Event                      : in out Button_Push_Event_Type) is
   pragma Unreferenced (Event);
   ---------------------------------------------------------------

--    Connection_Data   : Camera.Main.Window_Connection_Type'class renames
--                         Camera.Main.Window_Connection_Type'class (
--                            Ada_Lib.Test_States/Get_Window_Connection_Data.all);
--    Control_Card      : constant Control_Card_Class_Access :=
--                         Connection_Data.Get_Control_Card;
   begin
      Log_In (Debug);
not_implemented;
--    Pause_On_Flag ("control widget displayed", Here, Debug);
--    Control_Card.Get_Cancel_Button.Fire_On_Click;
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

      Test.Add_Optional_Routine (
--         Needs_Camera   => True,
         Routine        => Widgets.Control.Unit_Test.Test_Create_Control'access,
         Routine_Name   => "Test_Create_Control",
         Suite_Name     => Suite_Name);
      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
--    State                      : Configuration.Camera.State.State_Type renames
--                                  Test.State;
   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down);
--    State.Load (Options.Camera_Options.Location, State_Test_Path);
--    -- need to load state 1st
--    Test.Setup.Load (State, Setup_Test_Path);
      Camera.Lib.Unit_Test.With_Camera_With_GNOGA_Test_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);

   exception
      when Fault: Ada_Lib.Configuration.Failed =>
         Test_Type'class (Test).Set_Up_Message_Exception (Fault,
            "could not load configuration" &
            Quote (" Setup_Path", Setup_Test_Path) &
            Quote (" State_Path", State_Test_Path));
         raise;

      when Fault: others =>
         Trace_Exception (Debug or Trace_Set_Up_Tear_Down, Fault);
         Log_Exception (Debug or Trace_Set_Up_Tear_Down);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

--    Options     : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'class
--                   renames Camera.Lib.Unit_Test.
--                      Get_Camera_Unit_Test_Constant_Options.all;
--    Brand       : Standard.Camera.Brand_Type renames
--                   Options.Nested_Options.Brand;
      Test_Suite  : constant AUnit.Test_Suites.Access_Test_Suite :=
                     new AUnit.Test_Suites.Test_Suite;
      Tests       : constant Test_Access := new Test_Type (
--                   Brand             => Brand,
                     Initialize_GNOGA  => True);

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
      Log_In (Debug or Trace_Set_Up_Tear_Down);
      Camera.Lib.Unit_Test.With_Camera_With_GNOGA_Test_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up_Tear_Down);
   end Tear_Down;

   ----------------------------------------------------------------
   procedure Test_Create_Control (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
not_implemented;
--    declare
--       Connection_Data   : constant GNOGA_Ada_lib.
--                            Connection_Data_Class_Access :=
--                               Ada_Lib.Test_States/Get_Window_Connection_Data;
--       Cards                : constant Main.Cards_Access_Type :=
--                               Connection_Data.Get_Cards;
--       Tabs                 : constant Gnoga.Gui.View.Card.
--                               Pointer_To_Tab_Class :=
--                                  Connection_Data.Get_Tabs;
--    begin
--       Tabs.Select_Tab (Widget_Name);
--       Pause_On_Flag ("control tab displayed", Here, Debug);
--
--       declare
--          Current_Card            : constant Gnoga.Gui.View.
--                                     Pointer_To_View_Base_Class :=
--                                        Cards.Card (Widget_Name);
--       begin
--          Assert (Current_Card /= Null, "control card not found");
--          Assert (Current_Card.Visible, "control card not visible");
--       end;
--
--       declare
--          Event                   : Button_Push_Event_Type;
--
--       begin
--          Event.Start (
--             Description    => "button timer",
--             Dynamic        => False,
--             Wait           => 0.25);
--          delay 0.5;     -- wait for button to be pushed
--       end;
--    end;
      Log_Out (Debug);

   exception
      when Fault : others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);


   end Test_Create_Control;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Widgets.Control.Unit_Test;

