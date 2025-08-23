with Ada.Exceptions;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with Camera.Commands.PTZ_Optics;
with Camera.Lib.Unit_Test;
with Interfaces;
with Video.Lib;

package body Camera.Commands.Unit_Test is

   use type Interfaces.Integer_16;

   type Test_Type is new
                     Standard.Camera.Lib.Unit_Test.With_Camera_No_GNOGA_Test_Type (
         Brand       => Camera.Lib.PTZ_Optics_Camera) with null record;

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
   ) with post => Verify_Tear_Down (Test);

   function Get_Test_Preset
   return Video.Lib.Preset_ID_Type renames Video.Lib.Get_Default_Preset_ID;

-- procedure Test_Get_Absolute (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Set_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Debug       : Boolean renames Camera.Lib.Unit_Test.Camera_Commands_Debug;
   Suite_Name  : constant String := "Commands";

   ---------------------------------------------------------------
   procedure Check_Coordinates (
      Got_Pan                    : in     Absolute_Type;
      Expected_Pan               : in     Absolute_Type;
      Got_Tilt                   : in     Absolute_Type;
      Expected_Tilt              : in     Absolute_Type;
      From                       : in     String := Here) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan got " & Got_Pan'img & " tilt " & Got_Tilt'img &
               " expected pan " & Expected_Pan'img & " tilt " & Expected_Tilt'img &
               " from " & From);
      Assert (Got_Pan = Expected_Pan, "got wrong pan" & Got_Pan'img &
         " expected pan" & Expected_Pan'img);
      Assert (Got_Tilt = Expected_Tilt, "got wrong tilt" & Got_Tilt'img &
         " expected tilt" & Expected_Tilt'img);
      Log_Out (Debug);
   end Check_Coordinates;

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

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Get_Absolute'access,
--       Routine_Name   => AUnit.Format ("Test_Get_Absolute")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Relative'access,
         Routine_Name   => AUnit.Format ("Test_Position_Relative")));


      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Set_Absolute'access,
         Routine_Name   => AUnit.Format ("Test_Set_Absolute")));


      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Set_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Set_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Zoom'access,
         Routine_Name   => AUnit.Format ("Test_Zoom")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test           : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Standard.Camera.Lib.Unit_Test.With_Camera_No_GNOGA_Test_Type (Test).Set_Up;

      declare
         Speed       : constant Data_Type :=
                        Test.Camera_Info.Camera.Get_Camera_Speed (
                           Select_Maximum_Speed);
      begin
         Log_Here (Debug or Trace_Set_Up, "speed " & Speed'img);
         Test.Camera_Info.Camera.Set_Preset (Video.Lib.Get_Default_Preset_ID,
            Speed => Speed);     -- normally same as preset 0
      end;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " &
            Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   --------------------------------ult-------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
      Test_Suite  : constant AUnit.Test_Suites.Access_Test_Suite :=
                     new AUnit.Test_Suites.Test_Suite;
      Test        : constant Test_Access := new Test_Type;

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Test);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

      Speed          : constant Data_Type :=
                        Test.Camera_Info.Camera.Get_Camera_Speed (
                           Select_Maximum_Speed);
   begin
      Log_In (Debug or Trace_Set_Up, "speed " & Speed'img);
      Test.Camera_Info.Camera.Set_Preset (Get_Test_Preset,     -- normally same as preset 0
         Speed => Speed);
      Standard.Camera.Lib.Unit_Test.With_Camera_No_GNOGA_Test_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " &
            Ada.Exceptions.Exception_Message (Fault));

   end Tear_Down;

--   ----------------------------------------------------------------
--   procedure Test_Get_Absolute (
--      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--   pragma Unreferenced (Test);
--   ----------------------------------------------------------------
--
----    Local_Test                 : Test_Type'class renames Test_Type'class (Test);
--
--   begin
--      Log_In (Debug);
--      Assert (False, "not implemented");
--      Log_Out (Debug);
--   end Test_Get_Absolute;

   ----------------------------------------------------------------
   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

   -- moves camera around 4 steps until back to original location

      type Step_Index_Type    is range 1 .. 4;

      type Absolute_Step_Type is record
         Pan                  : Absolute_Type;
         Tilt                 : Absolute_Type;
      end record;

      type Relative_Step_Type is record
         Pan                  : Relative_Type;
         Tilt                 : Relative_Type;
      end record;

      Expected                : array (Step_Index_Type) of Absolute_Step_Type;
      Offset                  : constant := 100;
      Local_Test              : Test_Type'class renames Test_Type'class (Test);
      Final_Pan               : Absolute_Type;
      Final_Tilt              : Absolute_Type;
      Step                    : Step_Index_Type := 1;
      Test_Pan                : Absolute_Type;
      Test_Tilt               : Absolute_Type;
      Offsets                 : constant array (Step_Index_Type) of Relative_Step_Type := (
                                 (
                                    Pan   => Offset,
                                    Tilt  => 0
                                 ),
                                 (
                                    Pan   => 0,
                                    Tilt  => -Offset
                                 ),
                                 (
                                    Pan   => -Offset,
                                    Tilt  => 0
                                 ),
                                 (
                                    Pan   => 0,
                                    Tilt  => Offset
                                 )
                              );


   begin
      Log_In (Debug);
      -- start from Get_Test_Preset as reference - set by Set_Up
      Local_Test.Camera_Info.Camera.Set_Preset (Get_Test_Preset);
      -- move 4 steps return to Test_Prset
      -- get coordinats of test preset
      Local_Test.Camera_Info.Camera.Get_Absolute (Test_Pan, Test_Tilt);
      Log_Here (Debug, "test pan " & Test_Pan'img & " tilt " & Test_Tilt'img);

      for Offset of Offsets loop
      -- set relative
         Local_Test.Camera_Info.Camera.Position_Relative (
            Offset.Pan, Offset.Tilt);
         delay 0.2;
         if Debug then
            declare
               Expected_Entry    : Absolute_Step_Type renames Expected (Step);
               Pan               : Absolute_Type;
               Tilt              : Absolute_Type;

            begin
               if Step = 1 then
                  Expected_Entry.Pan := Test_Pan +
                     Absolute_Type (Offset.Pan);
                  Expected_Entry.Tilt := Test_Tilt +
                     Absolute_Type (Offset.Tilt);
               else
                  declare
                     Last_Entry  : Absolute_Step_Type renames Expected (Step - 1);
                  begin
                     Expected_Entry.Pan := Last_Entry.Pan +
                        Absolute_Type (Offset.Pan);
                     Expected_Entry.Tilt := Last_Entry.Tilt +
                        Absolute_Type (Offset.Tilt);
                  end;
               end if;
               Local_Test.Camera_Info.Camera.Get_Absolute (Pan, Tilt);
               Log_Here ("step" & Step'img &
                  " pan " & Pan'img & " offset " & Offset.Pan'img &
                  " tilt " & Tilt'img & " offset " & Offset.Tilt'img &
                  " expected pan" & Expected_Entry.Pan'img &
                  " Tilt" & Expected_Entry.Tilt'img);
            end;
         end if;
         if Step < Step_Index_Type'last then
            Step := Step + 1;
         end if;
      end loop;

      -- get coordinates of final location
      Local_Test.Camera_Info.Camera.Get_Absolute (Final_Pan, Final_Tilt);
      -- verify it got coordinates that were set
      Log_Here (Debug, "final pan " & Final_Pan'img & " tilt " & Final_Tilt'img);
      Check_Coordinates (Final_Pan, Test_Pan , Final_Tilt, Test_Tilt);
      -- set it back to reference
      Local_Test.Camera_Info.Camera.Set_Preset (Get_Test_Preset);
      -- get its coordinats
      Local_Test.Camera_Info.Camera.Get_Absolute (Final_Pan, Final_Tilt);
      Log_Here (Debug, "final pan " & Final_Pan'img & " tilt " & Final_Tilt'img);
      Check_Coordinates (Final_Pan, Test_Pan, Final_Tilt, Test_Tilt);
      Log_Out (Debug);

   exception

      when Fault: Camera.Commands.Timeout =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

   end Test_Position_Relative;

   ----------------------------------------------------------------
   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test  : Test_Type'class renames Test_Type'class (Test);
      Final_Pan   : Absolute_Type;
      Final_Tilt  : Absolute_Type;
      Pan_Set     : Absolute_Type;
      Pan_Step    : constant array (1 .. 2) of
                     Absolute_Type := (
                        100,
                        -100);
      Test_Pan    : Absolute_Type;
      Test_Tilt   : Absolute_Type;
      Tilt_Set    : Absolute_Type;
      Tilt_Step    : constant array (1 .. 2) of
                     Absolute_Type := (
                        -50,
                        50);

   begin
      Log_In (Debug);
      for Counter in 1 .. 2 loop
         Log_Here (Debug, "counter" & Counter'img);
         -- use Get_Test_Preset as reference
         Local_Test.Camera_Info.Camera.Set_Preset (Get_Test_Preset);
         -- get coordinats of test preset
         Local_Test.Camera_Info.Camera.Get_Absolute (Test_Pan, Test_Tilt);
         -- calculate offset from reference
         Pan_Set := Test_Pan + Pan_Step (Counter);
         Tilt_Set := Test_Tilt + Tilt_Step (Counter);
         -- set to that offset
         Local_Test.Camera_Info.Camera.Set_Absolute (Pan_Set, Tilt_Set);
         -- get coordinates of new location
         Local_Test.Camera_Info.Camera.Get_Absolute (Final_Pan, Final_Tilt);
         -- verify it got coordinates that were set
         Check_Coordinates (Final_Pan, Pan_Set, Final_Tilt, Tilt_Set);
         -- set it back to reference
         Local_Test.Camera_Info.Camera.Set_Preset (Get_Test_Preset);
         -- git its coordinats
         Local_Test.Camera_Info.Camera.Get_Absolute (Final_Pan, Final_Tilt);
         Check_Coordinates (Final_Pan, Test_Pan, Final_Tilt, Test_Tilt);
      end loop;
      Log_Out (Debug);
   end Test_Set_Absolute;

   ----------------------------------------------------------------
   procedure Test_Set_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames Test_Type'class (Test);
      Test_Pan                   : Absolute_Type;
      Pan                        : Absolute_Type;
      Pan_Set                    : Absolute_Type;
      Tilt                       : Absolute_Type;
      Test_Tilt                  : Absolute_Type;
      Tilt_Set                   : Absolute_Type;

   begin
      Log_In (Debug);
      -- use test preset as reference - set by Set_Up
      Local_Test.Camera_Info.Camera.Get_Absolute (Test_Pan, Test_Tilt);
      -- calculate some points relative to test preset
      Pan_Set := Test_Pan + 100;
      Tilt_Set := Test_Tilt - 100;
      -- set camera to those offsets
      Local_Test.Camera_Info.Camera.Set_Absolute (Pan_Set, Tilt_Set);
      -- check it was set to that point
      Local_Test.Camera_Info.Camera.Get_Absolute (Pan, Tilt);
      Check_Coordinates (Pan, Pan_Set, Tilt, Tilt_Set);
      Log_Out (Debug);
   end Test_Set_Preset;

   ----------------------------------------------------------------
   procedure Test_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      subtype Counter_Type
                  is Integer range 1 .. 2;
      type Pass_Type
                  is (Get_Settings, Test_Settings);
      Local_Test  : Test_Type'class renames Test_Type'class (Test);
      Modes       : constant array (Counter_Type) of
                     Camera.Commands.Zoom_Mode_Type := (
                        Camera.Commands.Minimum,
                        Camera.Commands.Maximum);
      Zoom_Values : array (Pass_Type, Counter_Type) of
                     Absolute_Type;

   begin
      Log_In (Debug);

      for Pass in Pass_Type'range loop
      -- 1st pass records original values
      -- 2nd pass compares values

         for Counter in Counter_Type'range loop
            -- 1st step sets min zoom
            -- 2nd step sets max zoom
            Log_Here (Debug, "pass " & Pass'img & " counter" & Counter'img);
            Local_Test.Camera_Info.Camera.Set_Fixed_Zoom (Modes (Counter));

            Local_Test.Camera_Info.Camera.Get_Zoom (Zoom_Values (Pass, Counter));
            Log_Here (Debug, "zoom value " & Zoom_Values (Pass, Counter)'img);

            if Pass = Test_Settings then
               Assert (Zoom_Values (Get_Settings, Counter) =
                  Zoom_Values (Test_Settings, Counter),
                  "recorded setting " &
                  Zoom_Values (Get_Settings, Counter)'img &
                  " does not matched test setting " &
                  Zoom_Values (Test_Settings, Counter)'img);
            end if;
         end loop;
      end loop;
      Log_Out (Debug);
   end Test_Zoom;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Camera.Commands.Unit_Test;
