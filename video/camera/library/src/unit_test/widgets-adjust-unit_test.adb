with Ada.Exceptions;
with GNOGA_Ada_Lib;
with Ada_Lib.Timer;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Camera.Commands;
with Camera.Lib.Unit_Test;
with Camera.Main;
with Ada_Lib.Test_States;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Gnoga.Gui.Base;
with Interfaces;
with Video.Lib;

package body Widgets.Adjust.Unit_Test is

   use type Interfaces.Integer_16;

   type Widgets_Adjust_Test_Type (
      Brand                      : Standard.Camera.Brand_Type) is new
                                    Camera.Lib.Unit_Test.
                                       With_Camera_With_GNOGA_Test_Type (
                                          Brand             => Brand,
                                          Initialize_GNOGA  => False) with
                                          -- Set_Up will use Main.Run to initialize
                                             null record;
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

-- overriding
-- procedure Tear_Down (
--    Test                       : in out Widgets_Adjust_Test_Type);

   procedure Test_Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   package Move_Package is

      type Mouse_Move_Event_Type is new Ada_Lib.Timer.Event_Type with record
         Connection_Data         : Camera.Main.Window_Connection_Class_Access;
         Mouse_Event             : Gnoga.Gui.Base.Mouse_Event_Record;
      end record;

      type Mouse_Move_Event_Access is access Mouse_Move_Event_Type;

      procedure Initialize_Event (
         Mouse_Move_Event        : in out Mouse_Move_Event_Type;
         Connection_Data         : in     GNOGA_Ada_lib.Connection_Data_Class_Access;
         Description             : in     String;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record;
         Wait                    : in     Duration);

      overriding
      procedure Callback (
         Event                   : in out Mouse_Move_Event_Type);

   end Move_Package;

   Debug       : Boolean renames Ada_Lib.Options.Ada_Lib_Widgets.Adjust_Debug;
   Suite_Name  : constant String := "Adjust_Card";

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

   begin
      Log_In (Debug or Trace_Set_Up_Tear_Down);
      Camera.Lib.Unit_Test.With_Camera_With_GNOGA_Test_Type (Test).Set_Up;
      Test.Camera_Info.Camera.Set_Preset (Video.Lib.Get_Default_Preset_ID);
      Log_Out (Debug or Trace_Set_Up_Tear_Down);

   exception
      when Fault: others =>
         Trace_Exception (Debug or Trace_Set_Up_Tear_Down, Fault);
         Assert (False, "exception message " &
            Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite
   return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'class
                                    renames Camera.Lib.Unit_Test.
                                       Get_Camera_Unit_Test_Constant_Options.all;
      Brand       : Standard.Camera.Brand_Type renames
                     Options.Camera_Library_Options.Camera_Options.Brand;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite
                                    := new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Widgets_Adjust_Test_Access :=
                                    new Widgets_Adjust_Test_Type (Brand);

   begin
      Log_In (Debug); --, "test state address " & Image (Tests.State'address) & " pointer address " & image (Read_Only_Global_Camera_State'address));
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

--   ---------------------------------------------------------------
--   overriding
--   procedure Tear_Down (
--      Test                       : in out Widgets_Adjust_Test_Type) is
--   ---------------------------------------------------------------
--
--   begin
--      Log_In (Debug);
----    GNOGA_Ada_Lib.Clear_Connection_Data;
--      Camera.Lib.Unit_Test.No_Camera_With_GNOGA_Test_Type (Test).Tear_Down ;
--      Log_Out (Debug);
--   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Mouse_Move (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

   use Gnoga.Gui.Base;
   begin
      Log_In (Debug);
      declare
         Local_Test        : Widgets_Adjust_Test_Type renames
                              Widgets_Adjust_Test_Type (Test);
         Connection_Data   : constant Camera.Main.Window_Connection_Class_Access :=
                              Camera.Main.Window_Connection_Class_Access (
                                 Ada_Lib.Test_States.Get_Window_Connection_Data);
         Camera            : Standard.Camera.Commands.Camera_Class_Access renames
                              Connection_Data.Get_Camera;
         Event             : constant Move_Package.Mouse_Move_Event_Access := new
                              Move_Package.Mouse_Move_Event_Type;

         Adjust_Card       : constant Adjust_Card_Access :=
                              Connection_Data.Get_Adjust_Card;
         Pan               : Standard.Camera.Absolute_Type;
         Pan_Offset        : constant := 100;
         Start_Pan         : Standard.Camera.Absolute_Type;
         Start_Tilt        : Standard.Camera.Absolute_Type;
         Tilt              : Standard.Camera.Absolute_Type;
         Tilt_Offset       : constant := 200;

      begin
         Log_Here (Debug);
         Camera.Get_Absolute (Start_Pan, Start_Tilt);
         declare
            Expected_Pan      : constant Standard.Camera.Absolute_Type :=
                                 Start_Pan + Pan_Offset;
            Expected_Tilt     : constant Standard.Camera.Absolute_Type :=
                                 Start_Tilt + Tilt_Offset;
         begin
            Log_Here (Debug,
               "start pan" & Start_Pan'img & " tilt" & Start_Tilt'img &
               " expected pan" & Expected_Pan'img & " tilt" & Expected_Tilt'img);
            -- create a mouse move event telling the amout to move
            Move_Package.Initialize_Event (Event.all,
               Connection_Data=> Gnoga_Ada_Lib.Connection_Data_Class_Access (
                                    Connection_Data),
               Description    => "mouse move event",
               Mouse_Event    => (
                  Message        => Mouse_Move,
                  X              => Pan_Offset,
                  Y              => Tilt_Offset,
                  Screen_X       => 100,
                  Screen_Y       => 200,
                  Left_Button    => False,
                  Middle_Button  => False,
                  Right_Button   => False,
                  Alt            => False,
                  Control        => False,
                  Shift          => False,
                  Meta           => False),
               Wait           => 0.25);

            Log_Here (Debug);
            Adjust_Card.Fire_On_Mouse_Click (Event.Mouse_Event);
            delay 0.5;     -- wait for button to be pushed
            Log_Here (Debug);
            Camera.Get_Absolute (Pan, Tilt);
            Assert (Pan = Expected_Pan and then Tilt = Expected_Tilt,
               "pan" & Pan'img & " expected" & Expected_Pan'img &
               " tilt" & Tilt'img & " expected" & Expected_Tilt'img);
         end;
      end;
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
         Mouse_Move_Event  : in out Mouse_Move_Event_Type;
         Connection_Data   : in     GNOGA_Ada_lib.Connection_Data_Class_Access;
         Description       : in     String;
         Mouse_Event       : in     Gnoga.Gui.Base.Mouse_Event_Record;
         Wait              : in     Duration) is
      ---------------------------------------------------------------

      begin
         Mouse_Move_Event.Start (Wait, Description,
            Dynamic     => True,
            Repeating   => False);
         Mouse_Move_Event.Mouse_Event := Mouse_Event;
      end Initialize_Event;

      ---------------------------------------------------------------
      overriding
      procedure Callback (
         Event                   : in out Mouse_Move_Event_Type) is
      ---------------------------------------------------------------

      Adjust_Card       : constant Adjust_Card_Access :=
                           Event.Connection_Data.Get_Adjust_Card;
      Cell              : constant Generic_Cell_Package.
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

