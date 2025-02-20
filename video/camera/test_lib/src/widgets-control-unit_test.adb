with Ada.Exceptions;
with Ada_Lib.Configuration;
with Ada_Lib.GNOGA;
with Ada_Lib.Timer;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Base;
with Configuration.Camera.State;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;
--with Configuration.State;
with Gnoga.Gui.View.Card;
with Main;

package body Widgets.Control.Unit_Test is

   use type Gnoga.Gui.View.Pointer_To_View_Base_Class;

   type Button_Push_Event_Type   is new Ada_Lib.Timer.Event_Type with null record;

   type Test_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type;
      Initialize_GNOGA           : Boolean) is new Camera.Lib.Unit_Test.
                                    Camera_Window_Test_With_Camera_Type (
                                       Brand             => Brand,
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
      Test                       : in out Test_Type);

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

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      Control_Card               : constant Control_Card_Access :=
                                    Connection_Data.Get_Control_Card;
   begin
      Log_In (Debug);
      Pause_On_Flag ("control widget displayed", Here, Debug);
      Control_Card.Get_Cancel_Button.Fire_On_Click;
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
         Routine        => Widgets.Control.Unit_Test.Test_Create_Control'access,
         Routine_Name   => AUnit.Format ("Test_Create_Control")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Options.all;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug or Trace_Set_Up);
      State.Load (Options.Camera_Options.Location, State_Test_Path);
      -- need to load state 1st
      Test.Setup.Load (State, Setup_Test_Path);
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: Ada_Lib.Configuration.Failed =>
         Test.Set_Up_Message_Exception (Fault, "could not load configuration" &
            Quote (" Setup_Path", Setup_Test_Path) &
            Quote (" State_Path", State_Test_Path));
         raise;

      when Fault: others =>
         Trace_Exception (Debug or Trace_Set_Up, Fault);
         Log_Exception (Debug or Trace_Set_Up);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'class
                                    renames Camera.Lib.Unit_Test.Options.all;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type (
                                    Brand    => Options.Camera_Options.Brand,
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
      Log_In (Debug);
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Tear_Down;
      Log_Out (Debug);
   end Tear_Down;

   ----------------------------------------------------------------
   procedure Test_Create_Control (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
--    if not Ada_Lib.Options.Actual.Program_Options_Type (
--          Ada_Lib.Options.Ada_Lib.Options..all).Test_Driver then
         Log_Here (Debug);
         declare
--          Local_Test           : Test_Type'class renames
--                                 Test_Type'class (Test);
            Connection_Data      : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
            Cards                : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
            Tabs                 : constant Gnoga.Gui.View.Card.
                                    Pointer_To_Tab_Class :=
                                       Connection_Data.Get_Tabs;
         begin
            Tabs.Select_Tab (Widget_Name);
            Pause_On_Flag ("control tab displayed", Here, Debug);

            declare
               Current_Card            : constant Gnoga.Gui.View.
                                          Pointer_To_View_Base_Class :=
                                             Cards.Card (Widget_Name);
            begin
               Assert (Current_Card /= Null, "control card not found");
               Assert (Current_Card.Visible, "control card not visible");
            end;

            declare
               Event                   : Button_Push_Event_Type;

            begin
               Event.Initialize (0.25, "button timer");
               delay 0.5;     -- wait for button to be pushed
            end;
         end;
--    end if;
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

