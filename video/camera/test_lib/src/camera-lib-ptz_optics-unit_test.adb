--with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada_Lib.GNOGA;
--with ADA_LIB.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with Base;
--with Camera.Lib.Base;
with Camera.Lib.Unit_Test;
with Interfaces;

package body Camera.Lib.PTZ_Optics.Unit_Test is

   use type Interfaces.Integer_16;
-- use type Camera.Lib.Base.Base_Camera_Class_Access;
   use type Camera.Command_Queue.Queued_Camera_Class_Access;
   use type Preset_ID_Type;

   type Test_Type (
      Brand          : Camera.Lib.Brand_Type) is new Camera.Lib.Unit_Test.
                     Camera_Test_Type (Brand) with null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (Test : Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

-- overriding
-- procedure Set_Up (
--    Test                       : in out Test_Type
-- ) with Post => Test.Verify_Set_Up;

-- overriding
-- procedure Tear_Down (
--    Test                       : in out Test_Type
-- ) with post => Verify_Torn_Down (Test);

-- procedure Test_Get_Absolute (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   function Have_Camera (
      Test  : in     AUnit.Test_Cases.Test_Case'class
   ) return Boolean;

   procedure Test_Get_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Have_Camera (Test);

   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Have_Camera (Test);

   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Have_Camera (Test);

   procedure Test_Move_To_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Have_Camera (Test);

   procedure Test_Set_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Have_Camera (Test);

   procedure Test_Update_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   Suite_Name                    : constant String := "Commands";
   Test_Preset                   : constant := Camera.Lib.PTZ_Optics.
                                    Max_Preset;

   ---------------------------------------------------------------
   procedure Check_Coordinates (
      Got_Pan                    : in     Absolute_Type;
      Expected_Pan               : in     Absolute_Type;
      Got_Tilt                   : in     Absolute_Type;
      Expected_Tilt              : in     Absolute_Type) is
   ---------------------------------------------------------------

   begin
      Assert (Got_Pan = Expected_Pan, "got wrong pan" & Got_Pan'img &
         " expected pan" & Expected_Pan'img);
      Assert (Got_Tilt = Expected_Tilt, "got wrong tilt" & Got_Tilt'img &
         " expected tilt" & Expected_Tilt'img);
   end Check_Coordinates;

   ---------------------------------------------------------------
   function Have_Camera (
      Test  : in     AUnit.Test_Cases.Test_Case'class
   ) return Boolean is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      return Local_Test.Camera_Queue /= Null;
   end Have_Camera;
   ---------------------------------------------------------------

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
         Routine        => Test_Get_Zoom'access,
         Routine_Name   => AUnit.Format ("Test_Get_Zoom")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Relative'access,
         Routine_Name   => AUnit.Format ("Test_Position_Relative")));


      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Set_Absolute'access,
         Routine_Name   => AUnit.Format ("Test_Set_Absolute")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Move_To_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Move_To_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Update_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Set_Zoom'access,
         Routine_Name   => AUnit.Format ("Test_Set_Zoom")));

      Log_Out (Debug);
   end Register_Tests;

-- ---------------------------------------------------------------
-- overriding
-- procedure Set_Up (
--    Test              : in out Test_Type) is
-- ---------------------------------------------------------------
--
--    Options           : Standard.Camera.Lib.Unit_Test.
--                         Camera_Lib_Unit_Test_Options_Type'class
--                            renames Standard.Camera.Lib.Unit_Test.
--                               Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
--    Brand             : Standard.Camera.Lib.Brand_Type renames
--                         Options.Camera_Options.Brand;
--
-- begin
--    Log_In (Debug or Trace_Set_Up);
--    case Brand is
--
--       when Standard.Camera.Lib.ALPTOP_Camera =>
--          Test.Camera := Test.ALPTOP'access;
--
--       when Standard.Camera.Lib.No_Camera =>
--          raise Failed with "no camera brand selected";
--
--       when Standard.Camera.Lib.PTZ_Optics_Camera =>
--          Test.Camera := Test.PTZ_Optics'access;
--
--    end case;
--    Log_Out (Debug or Trace_Set_Up);
--
-- exception
--    when Fault: others =>
--       Trace_Exception (Debug, Fault);
--       Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));
--
-- end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Brand                      : Standard.Camera.Lib.Brand_Type renames
                                    Options.Camera_Options.Brand;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Test                       : constant Test_Access := new Test_Type (Brand);

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Test);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

-- ---------------------------------------------------------------
-- overriding
-- procedure Tear_Down (
--    Test                       : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
--
-- exception
--    when Fault: others =>
--       Trace_Exception (Debug, Fault);
--       Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));
--
-- end Tear_Down;

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
   procedure Test_Get_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames Test_Type'class (Test);
      Zoom                       : Absolute_Type;

   begin
      Log_In (Debug);
      Local_Test.Camera_Queue.Get_Zoom (Zoom);
      Log_Here (Debug, "zoom" & Zoom'img);
      Log_Out (Debug);

   exception

      when Error: others =>
         Log_Exception (Debug, Error);
         Ada_Lib.Unit_Test.Exception_Assert (Error);

   end Test_Get_Zoom;

   ----------------------------------------------------------------
   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames Test_Type'class (Test);
      Final_Pan         : Absolute_Type;
      Final_Tilt        : Absolute_Type;
      Pan_Offset        : constant := 100;
      Test_Pan           : Absolute_Type;
      Test_Tilt           : Absolute_Type;
      Tilt_Offset        : constant := -100;

   begin
      Log_In (Debug);
      -- use Test_Preset as reference
      Local_Test.Camera_Queue.Move_To_Preset (Test_Preset);
      -- get coordinats of test preset
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Test_Pan, Test_Tilt);
      -- set relative
      Local_Test.Camera_Queue.Position_Relative (Pan_Offset, Tilt_Offset);
      -- get coordinates of new location
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Final_Pan, Final_Tilt);
      -- verify it got coordinates that were set
      Check_Coordinates (Final_Pan, Test_Pan + Pan_Offset,
         Final_Tilt, Test_Tilt + Tilt_Offset);
      -- set it back to reference
      Local_Test.Camera_Queue.Move_To_Preset (Test_Preset);
      -- git its coordinats
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Final_Pan, Final_Tilt);
      Check_Coordinates (Final_Pan, Test_Pan, Final_Tilt, Test_Tilt);
      Log_Out (Debug);

   exception

      when Error: others =>
         Log_Exception (Debug, Error);
         Ada_Lib.Unit_Test.Exception_Assert (Error);

   end Test_Position_Relative;

   ----------------------------------------------------------------
   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test        : Test_Type'class renames Test_Type'class (Test);
      Default_Preset    : constant Preset_ID_Type :=
                           Local_Test.Camera_Queue.Get_Default_Preset;
      Final_Pan         : Absolute_Type;
      Final_Tilt        : Absolute_Type;
      Options           : Standard.Camera.Lib.Unit_Test.
                           Camera_Lib_Unit_Test_Options_Type'class
                              renames Standard.Camera.Lib.Unit_Test.
                                 Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Pan_Set           : Absolute_Type;
      Test_Pan           : Absolute_Type;
      Test_Tilt           : Absolute_Type;
      Tilt_Set          : Absolute_Type;

   begin
      Log_In (Debug);
      -- use Test_Preset as reference
      Local_Test.Camera_Queue.Move_To_Preset (Default_Preset);
      -- get coordinats of test preset
      Pause (Options.Manual, "camera set to default");
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Test_Pan, Test_Tilt);
      -- calculate offset from reference
      Pan_Set := Test_Pan + 100;
      Tilt_Set := Test_Tilt - 100;
      -- set to that offset
      Log_Here (Debug, "set pan " & Pan_Set'img & " tilt " & Tilt_Set'img);
      Local_Test.Camera_Queue.Set_Absolute (Pan_Set, Tilt_Set);
      if Options.Manual then
         Pause ("camera set to offset from default");
      end if;
      -- get coordinates of new location
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Final_Pan, Final_Tilt);
      Log_Here (Debug, "got pan " & Final_Pan'img & " tilt " & Final_Tilt'img);
      -- verify it got coordinates that were set
      Check_Coordinates (Final_Pan, Pan_Set, Final_Tilt, Tilt_Set);
      -- set it back to reference
      Local_Test.Camera_Queue.Move_To_Preset (Default_Preset);
      -- git its coordinats
      if Options.Manual then
         Pause ("camera set to default");
      end if;
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Final_Pan, Final_Tilt);
      Check_Coordinates (Final_Pan, Test_Pan, Final_Tilt, Test_Tilt);
      Log_Out (Debug);

   exception

      when Error: others =>
         Log_Exception (Debug, Error);
         Ada_Lib.Unit_Test.Exception_Assert (Error);

   end Test_Set_Absolute;

   ----------------------------------------------------------------
   procedure Test_Move_To_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);
      Default_Preset             : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Get_Default_Preset;
      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Test_Preset                : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Last_Preset;

   begin
      Log_In (Debug, "Test_Preset" & Test_Preset'img);
      Pause (Options.Manual, "set preset" & Test_Preset'img);
      Local_Test.Camera_Queue.Move_To_Preset (Default_Preset);
      if Options.Manual then
         Assert (Ask_Pause (True, "check default preset" & Default_Preset'img),
            "move to preset" & Default_Preset'img & " failed");
      end if;
      Local_Test.Camera_Queue.Move_To_Preset (Test_Preset);

      Assert (Ask_Pause (Options.Manual,
         "verify that the preset" & Test_Preset'img),
         "manual set failed");
      Log_Out (Debug);

   exception
      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

   end Test_Move_To_Preset;

   ---------------------------------------------------------------
   procedure Test_Update_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);
      Default_Preset             : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Get_Default_Preset;
      Minimum_Test_Preset        : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Minimum_Test_Preset;
      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Test_Preset                : constant  Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Last_Preset;
      Alternate_Preset           : constant Preset_ID_Type := Default_Preset + 1;

   begin
      Log_In (Debug, "Test_Preset" & Test_Preset'img &
         " saved preset" & Default_Preset'img &
         " Minimum_Test_Preset" & Minimum_Test_Preset'img);
      if Test_Preset < Minimum_Test_Preset then
         raise Failed with "tried to set non testing preset id" &
            Test_Preset'img & ". Less than minimum for testing" &
            Minimum_Test_Preset'img;
      end if;
      Local_Test.Camera_Queue.Move_To_Preset (Default_Preset);
      if Options.Manual then
         Assert (Ask_Pause (True, "check default preset" & Default_Preset'img),
            "move to preset" & Default_Preset'img & " failed");
      else
         Put_Line ("camera moved to default preset" & Default_Preset'img);
      end if;
      Local_Test.Camera_Queue.Update_Preset (Test_Preset);
      Put_Line ("preset" & Test_Preset'img &
         " saved location" & Default_Preset'img);
      Local_Test.Camera_Queue.Move_To_Preset (Alternate_Preset);
      if Options.Manual then
         Assert (Ask_Pause (True, "check alternate preset" & Alternate_Preset'img),
            "move to preset" & Alternate_Preset'img & " failed");
      else
         delay 2.5;  -- wait for camera to move
         Put_Line ("camera moved to alternate preset" & Alternate_Preset'img);
      end if;
      Local_Test.Camera_Queue.Move_To_Preset (Test_Preset);
      if Options.Manual then
         Assert (Ask_Pause (True, "check test preset" & Test_Preset'img &
            ". Should be same as" & Default_Preset'img),
            "move to preset" & Test_Preset'img & " failed");
      else
         Put_Line ("camera moved to test preset" & Test_Preset'img);
      end if;

      Log_Out (Debug);
   end Test_Update_Preset;


   ----------------------------------------------------------------
   procedure Test_Set_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Assert (False, "not implemented");
      Log_Out (Debug);

   exception

      when Error: others =>
         Log_Exception (Debug, Error);
         Ada_Lib.Unit_Test.Exception_Assert (Error);

   end Test_Set_Zoom;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Camera.Lib.PTZ_Optics.Unit_Test;
