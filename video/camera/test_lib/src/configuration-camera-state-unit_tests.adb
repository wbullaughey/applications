with Ada.Exceptions;
--with Ada_Lib.GNOGA;
with Ada_Lib.Options.Actual;
with Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
--with Base;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with Camera.Lib.Unit_Test;
--with Hex_IO;
with Video.Lib;

package body Configuration.Camera.State.Unit_Tests is

   use type Ada_Lib.Strings.String_Access;
-- use type Standard.Camera.Preset_ID_Type;
-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;
   use type Video.Lib.Preset_ID_Type;

   type Configuration_Load_Test_Type is new Standard.Camera.Lib.
                                    Unit_Test.With_Camera_No_GNOGA_Test_Type with
                                       null record;

   type Configuration_Load_Test_Access is access Configuration_Load_Test_Type;

   overriding
   function Name (
      Test                       : in     Configuration_Load_Test_Type
   ) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Load_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Configuration_Load_Test_Type
   ) with Post => Test.Verify_Set_Up;

   type Configuration_Tests_Type is new
                  Standard.Camera.Lib.Unit_Test.With_Camera_No_GNOGA_Test_Type
                     with null record;

   type Configuration_Tests_Access is access Configuration_Tests_Type;

   overriding
   function Name (
      Test                       : in     Configuration_Tests_Type
   ) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Tests_Type);

   overriding
   procedure Tear_Down (
      Test                       : in out Configuration_Tests_Type);

   procedure Test_Load (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;

   procedure Test_Values (
      Test                       : in out AUnit.Test_Cases.Test_Case'class
   ) with Pre => GNOGA_Ada_Lib.Has_Connection_Data;

-- type Connection_Data_Type     is new GNOGA_Ada_Lib.Connection_Data_Type
--                                  with null record;
   Suite_Name                    : constant String := "State";

   Test_State               : constant String :=
                                    "test_state.cfg";

 ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Configuration_Load_Test_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

 ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Configuration_Tests_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Load_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Load'access,
         Routine_Name   => AUnit.Format ("Test_Load")));

      Log_Out (Debug);

   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Tests_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Values'access,
         Routine_Name   => AUnit.Format ("Test_Values")));

      Log_Out (Debug);

   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                       : in out Configuration_Load_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Load_State := False;
      Standard.Camera.Lib.Unit_Test.With_Camera_No_GNOGA_Test_Type (Test).Set_Up ;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug or Trace_Set_Up, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite
   return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options     : Standard.Camera.Lib.Unit_Test.
                     Unit_Test_Program_Options_Type'class
                        renames Standard.Camera.Lib.Unit_Test.
                           Get_Camera_Unit_Test_Constant_Options.all;
      Test_Suite  : constant AUnit.Test_Suites.Access_Test_Suite
                     := new AUnit.Test_Suites.Test_Suite;
      Load_Test   : constant Configuration_Load_Test_Access :=
                     new Configuration_Load_Test_Type (
                        Brand       => Options.Camera_Options.Brand);
      Tests       : constant Configuration_Tests_Access :=
                     new Configuration_Tests_Type (Options.Camera_Options.Brand);

   begin
      Log_In (Debug, Quote ("suite", Suite_Name));
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Load_Test);
      Test_Suite.Add_Test (Tests);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

   ---------------------------------------------------------------
   overriding
   procedure Tear_Down (
      Test                       : in out Configuration_Tests_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      GNOGA_Ada_Lib.Clear_Connection_Data;
      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
      Log_Out (Debug or Trace_Set_Up);
   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Load (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

--    Connection_Data            : Base.Connection_Data_Type renames
--                                  Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
      Local_Test                 : Configuration_Tests_Type renames
                                    Configuration_Tests_Type (Test);
      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
--    State                      : Configuration.Camera.State.State_Type renames
--                                  Connection_Data.State;
   begin
      Log_In (Debug);
      Local_Test.State.Load (Options.Camera_Options.Location, Test_State);
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug);
         Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Test_Load;

   ---------------------------------------------------------------
   procedure Test_Values (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

--    Connection_Data            : Base.Connection_Data_Type renames
--                                  Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
      begin
         Log_In (Debug);
         declare
            Expected_Number_Columns    : constant := 3;
            Expected_Number_Rows       : constant := 4;
            Expected_Number_Configurations
                                       : constant := 4;
            Expected_Last_Presets      : constant Video.Lib.Preset_ID_Type :=
                                          Video.Lib.Constructor (5);

      --    type Preset_Value_Type     is record
      --       Row                     : Row_Type := Not_Set;
      --       Column                  : Column_Type := Not_Set;
      --    end record;

      --    Null_Preset_ID                : constant Preset_Value_Type := Preset_Value_Type'(
      --       Row      =>  Not_Set,
      --       Column   =>  Not_Set);

            type Expect_Image_Type     is array (Row_Type range 1 .. Expected_Number_Rows,
                                             Column_Type range 1 .. Expected_Number_Columns) of
                                          Ada_Lib.Strings.String_Access;

      --    type Expected_Preset_Type  is array (Preset_ID_Type range 1 ..
      --                                  Expected_Last_Presets) of Preset_Value_Type;

            Expected_Images            : constant Expect_Image_Type := Expect_Image_Type'[
                                             1 => [
                                                1 => new String'("preset_0.png"),
                                                2 => new String'("preset_5.png"),
                                                others => Null],
                                             2 => [
                                                2 => new String'("preset_2.png"),
                                                others => Null],
                                             3 => [
                                                2 => new String'("preset_4.png"),
                                                3 => new String'("preset_1.png"),
                                                others => Null],
                                             4 => [
                                                2 => new String'("preset_3.png"),
                                                others => Null]];

      --    Expected_Prsets            : constant Expected_Preset_Type :=
      --                                  Expected_Preset_Type'(
      --                                     1  => [ 1, 1 ],
      --                                     3  => [ 1, 2 ],
      --                                     5  => [ 1, 3 ],
      --                                     others => Null_Preset_ID ];
            Local_Test                 : Configuration_Tests_Type renames
                                          Configuration_Tests_Type (Test);
            Options                    : Standard.Camera.Lib.Unit_Test.
                                          Unit_Test_Program_Options_Type'class
                                             renames Standard.Camera.Lib.Unit_Test.
                                                Get_Camera_Unit_Test_Constant_Options.all;
            State                      : Configuration.Camera.State.State_Type
                                          renames Local_Test.State;
         begin
            Log_Here (Debug, "set " & State.Is_Loaded'img & " Number_Columns " &
               " location " & Options.Camera_Options.Location'img &
               " address " & Image (State.Number_Columns'address) &
               " bits " & State.Number_Columns'size'img);
--    Hex_IO.Dump_32 (State.Number_Columns'address, 32, 1, "number columns");
      --log_here ("test state address " & image (state'address) & " global state " & image (Global_Camera_State.all'address) & " pointer address " & image (Global_Camera_State'address));

            declare
               Number_Columns          : constant Column_Type :=
                                             State.Number_Columns;
               Number_Configurations   : constant Configuration_ID_Type :=
                                          State.Get_Number_Configurations;
               Last_Preset             : constant Standard.Camera.Preset_ID_Type :=
                                          Video.Lib.Get_Last_Preset_ID;
               Number_Rows             : constant Row_Type := State.Number_Rows;

            begin
               Log_Here (Debug,
                  "Number_Columns" & Number_Columns'img &
                  " Number_Configurations" & Number_Configurations'img &
                  " Last_Preset" & Last_Preset'img &
                  " Number_Rows" & Number_Rows'img);

               Assert (Number_Columns = Expected_Number_Columns,
                  "Number_Columns" & Number_Columns'img &
                  " not equal Expected_Number_Columns" &
                  Expected_Number_Columns'img);

               Assert (Number_Configurations = Expected_Number_Configurations,
                  "Number_Configurations" & Number_Configurations'img &
                  " not equal Expected_Number_Configurations" &
                  Expected_Number_Configurations'img);

               Assert (Last_Preset = Expected_Last_Presets,
                  "Last_Preset" & Last_Preset'img &
                  " not equal Expected_Last_Presets" &
                  Expected_Last_Presets'img);

               Assert (Number_Rows = Expected_Number_Rows,
                  "Number_Rows" & Number_Rows'img &
                  " not equal Expected_Number_Rows" &
                  Expected_Number_Rows'img);

               for Row in 1 .. Number_Rows loop
                  for Column in 1 .. Number_Columns loop
                     declare
                        Have_Image     : constant Boolean := State.
                                          Has_Image (Row, Column);
                        Value          : Ada_Lib.Strings.String_Access renames
                                          Expected_Images (Row, Column);
                     begin
                        if Value = Null then
                           Assert (not Have_Image, "had unexpected image for row" &
                              Row'img & " column" & Column'img);
                        elsif Have_Image then
      --log_here;
                           declare
                              Image    : constant String := State.
                                          Image_Path (Row, Column);
                           begin
                              Assert (Image = Value.all,
                                 "wrong image. " & Quote ("expected", Value.all) &
                                 Quote (" got", Image));
                           end;
                        else
                           Assert (False, "did not have expected image for row" &
                              Row'img & " column" & Column'img);
                        end if;
                     end;
                  end loop;
               end loop;
            end;
         end;
         Log_Out (Debug);

      exception

         when Fault: others =>
            Trace_Exception (Debug, Fault);
            Log_Exception (Debug);
            Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Test_Values;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Configuration.Camera.State.Unit_Tests;
