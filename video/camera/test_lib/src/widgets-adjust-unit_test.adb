with Ada.Exceptions;
with Ada_Lib.GNOGA;
with Ada_Lib.Timer;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Base;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Gnoga.Gui.Base;
with Main;

package body Widgets.Adjust.Unit_Test is

   type Widgets_Adjust_Test_Type (
      Brand                      : Camera.Lib.Brand_Type) is new
                                    Camera.Lib.Unit_Test.Camera_Window_Test_Type (
                                       Initialize_GNOGA  => True,
                                       Run_Main          => True) with record
      Setup                      : Configuration.Camera.Setup.Setup_Type;
   end record;

   type Widgets_Adjust_Test_Access
                                 is access Widgets_Adjust_Test_Type;

   overriding
   function Name (
      Test                       : in     Widgets_Adjust_Test_Type
   ) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Widgets_Adjust_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Widgets_Adjust_Test_Type
   ) with Post => Test.Verify_Set_Up;

   overriding
   procedure Tear_Down (
      Test                       : in out Widgets_Adjust_Test_Type);

   procedure Test_Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   package Move_Package is

      type Mouse_Move_Event_Type is new Ada_Lib.Timer.Event_Type with record
         Mouse_Event                : Gnoga.Gui.Base.Mouse_Event_Record;
      end record;

      procedure Initialize_Event (
         Mouse_Move_Evet         : in out Mouse_Move_Event_Type;
         Connection_Data         : in     Base.Connection_Data_Class_Access;
         Description             : in     String;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record;
         Wait                    : in     Duration);

      overriding
      procedure Callback (
         Event                   : in out Mouse_Move_Event_Type);

   end Move_Package;

   State_Test_Path               : constant String := "adjust_state.cfg";
   Setup_Test_Path               : constant String := "adjust_setup.cfg";
   Suite_Name                   : constant String := "Adjust_Card";

   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Widgets_Adjust_Test_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Widgets_Adjust_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Mouse_Move'access,
         Routine_Name   => AUnit.Format ("Test_Mouse_Move")));

      Log_Out (Debug);

   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Widgets_Adjust_Test_Type) is
   ---------------------------------------------------------------

      Options                 : Standard.Camera.Lib.Options_Type'class
                                 renames Standard.Camera.Lib.Unit_Test.Options.all;
   begin
      Log_In (Debug);
      Test.State.Load_Camera_State (
         Options.Location, State_Test_Path); -- need to load state 1st
      Test.Setup.Load (Test.State, Setup_Test_Path);
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Set_Up ;
--    Mouse_Move_Event_Type.Reset_Event;
      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " &
            Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite
   return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Camera.Lib.Options_Type'class
                                    renames Camera.Lib.Unit_Test.Options.all;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite
                                    := new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Widgets_Adjust_Test_Access :=
                                    new Widgets_Adjust_Test_Type (Options.Brand);

   begin
      Log_In (Debug); --, "test state address " & Image (Tests.State'address) & " pointer address " & image (Global_Camera_State'address));
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Widgets_Adjust_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
--    Ada_Lib.GNOGA.Clear_Connection_Data;
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Tear_Down ;
      Log_Out (Debug);
   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

--    use Gnoga.Gui.Base;

--    Local_Test        : Widgets_Adjust_Test_Type renames
--                         Widgets_Adjust_Test_Type (Test);
--    Connection_Data   : constant Base.Connection_Data_Access :=
--                            Base.Connection_Data_Access (
--                               Ada_Lib.GNOGA.Get_Connection_Data);
--    Description       : aliased String := "mouse move event";
--    Wait              : aliased Constant Duration := 0.25;
--    Event             : Move_Package.Mouse_Move_Event_Type; --  := Initialize_Event (
--                            Connection_Data=> Connection_Data,
--                            Description    => "mouse move event",
--                            Mouse_Event    => (
--                               Message        => Mouse_Move,
--                               X              => 100,
--                               Y              => 200,
--                               Screen_X       => 100,
--                               Screen_Y       => 200,
--                               Left_Button    => False,
--                               Middle_Button  => False,
--                               Right_Button   => False,
--                               Alt            => False,
--                               Control        => False,
--                               Shift          => False,
--                               Meta           => False),
--                            Wait           => 0.25);
--                         Description_Pointer  => Description'unchecked_Access,
--                         Offset               => Wait'unchecked_Access,
--                         Connection_Data      => Connection_Data,
--    Event             : Mouse_Move_Event_Type (
--                         Description_Pointer  => Description'unchecked_Access,
--                         Offset               => Wait'unchecked_Access,
--                         Connection_Data      => Connection_Data,
--                         Mouse_Event => (
--                            Message        => Mouse_Move,
--                            X              => 100,
--                            Y              => 200,
--                            Screen_X       => 100,
--                            Screen_Y       => 200,
--                            Left_Button    => False,
--                            Middle_Button  => False,
--                            Right_Button   => False,
--                            Alt            => False,
--                            Control        => False,
--                            Shift          => False,
--                            Meta           => False));

--    Adjust_Card                : constant Adjust_Card_Class_Access :=
--                                  Connection_Data.Get_Adjust_Card;
   begin
      Log_In (Debug);
--                            Connection_Data=> Connection_Data,
--                            Description    => "mouse move event",
--                            Mouse_Event    => (
--                               Message        => Mouse_Move,
--                               X              => 100,
--                               Y              => 200,
--                               Screen_X       => 100,
--                               Screen_Y       => 200,
--                               Left_Button    => False,
--                               Middle_Button  => False,
--                               Right_Button   => False,
--                               Alt            => False,
--                               Control        => False,
--                               Shift          => False,
--                               Meta           => False),
--                            Wait           => 0.25);
--    Event.Connection_Data := Connection_Data;
--    Event.Mouse_Event := Mouse_Event_Record'(
--       Message        => Mouse_Move,
--       X              => 100,
--       Y              => 200,
--       Screen_X       => 100,
--       Screen_Y       => 200,
--       Left_Button    => False,
--       Middle_Button  => False,
--       Right_Button   => False,
--       Alt            => False,
--       Control        => False,
--       Shift          => False,
--       Meta           => False);

      delay 0.5;     -- wait for button to be pushed
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug);
         Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Test_Mouse_Move;

   package body Move_Package is

      ---------------------------------------------------------------
      procedure Initialize_Event (
         Mouse_Move_Evet         : in out Mouse_Move_Event_Type;
         Connection_Data         : in     Base.Connection_Data_Class_Access;
         Description             : in     String;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record;
         Wait                    : in     Duration) is
      ---------------------------------------------------------------

      begin
         Mouse_Move_Evet.Initialize (Wait, Description,
            Dynamic     => False,
            Repeating   => False);
         Mouse_Move_Evet.Mouse_Event := Mouse_Event;
      end Initialize_Event;

      ---------------------------------------------------------------
      overriding
      procedure Callback (
         Event                   : in out Mouse_Move_Event_Type) is
      ---------------------------------------------------------------

         Connection_Data            : Base.Connection_Data_Type renames
                                       Base.Connection_Data_Type (
                                          Ada_Lib.GNOGA.Get_Connection_Data.all);
         Adjust_Card                : constant Adjust_Card_Access :=
                                       Connection_Data.Main_Data.Get_Adjust_Card;
         Cell                       : constant Generic_Cell_Package.
                                       Cell_Class_Access :=
                                          Adjust_Card.Get_Cell (
                                             Center_Column, Center_Row);
      begin
         Log_In (Debug, "event " & Event.Mouse_Event.Message'img);

         Cell.Fire_On_Mouse_Move (Event.Mouse_Event);
   --    Event.Set_Event;
         Log_Out (Debug);
      end Callback;

   end Move_Package;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Widgets.Adjust.Unit_Test;

