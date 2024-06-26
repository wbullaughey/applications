with Ada.Exceptions;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;

package body Camera.Commands.Unit_Test is

   procedure Test_Get_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Set_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Set_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

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
         Routine        => Test_Get_Absolute'access,
         Routine_Name   => AUnit.Format ("Test_Get_Absolute")));


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
         Routine        => Test_Set_Zoom'access,
         Routine_Name   => AUnit.Format ("Test_Set_Zoom")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Test                       : constant Test_Access := new Test_Type;

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

   begin
      Log_In (Debug);
      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Tear_Down;

   ----------------------------------------------------------------
   procedure Test_Get_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Test_Get_Absolute;

   ----------------------------------------------------------------
   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Test_Position_Relative;

   ----------------------------------------------------------------
   procedure Test_Set_Absolute (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Test_Set_Absolute;

   ----------------------------------------------------------------
   procedure Test_Set_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Test_Set_Preset;

   ----------------------------------------------------------------
   procedure Test_Set_Zoom (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames Test_Type'class (Test);

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Test_Set_Zoom;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Camera.Commands.Unit_Test;
