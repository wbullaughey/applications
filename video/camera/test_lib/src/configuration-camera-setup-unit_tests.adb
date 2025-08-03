with Ada.Exceptions;
with Ada_Lib.Directory.Compare_Files;
with GNOGA_Ada_Lib;
--with Ada_Lib.Options;
with Ada_Lib.Strings.Unlimited; use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Base;
--with Camera.Commands.PTZ_Optics;
with Camera.Lib.Unit_Test;
with Video_Lib;

--with Configuration.State;

package body Configuration.Camera.Setup.Unit_Tests is

   use type Standard.Camera.Preset_ID_Type;

   type Configuration_Load_Test_Type is new Standard.Camera.Lib.
                                    Unit_Test.With_Camera_Test_Type with
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

   type Configuration_Tests_Type is new Standard.Camera.Lib.
                                    Unit_Test.With_Camera_Test_Type with
                                       null record;

   type Configuration_Tests_Access is access Configuration_Tests_Type;

   overriding
   function Name (
      Test                       : in     Configuration_Tests_Type
   ) return Standard.AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Configuration_Tests_Type);

-- overriding
-- procedure Set_Up (
--    Test                       : in out Configuration_Tests_Type
-- ) with Post => Test.Verify_Set_Up;

-- overriding
-- procedure Tear_Down (
--    Test                       : in out Configuration_Tests_Type);

   procedure Test_Load (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Update (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Values (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- Camera_Description            : aliased constant String := "test camera";
   Suite_Name                    : constant String := "Setup";

-- Test_Setup               : constant String :=
--                                  "test_setup.cfg";
-- Test_State               : constant String :=
--                                  "test_state.cfg";

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

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update'access,
         Routine_Name   => AUnit.Format ("Test_Update")));

      Log_Out (Debug);

   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test                    : in out Configuration_Load_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Test.Load_State := False;
      Standard.Camera.Lib.Unit_Test.With_Camera_Test_Type (Test).Set_Up;
      Log_Out (Debug or Trace_Set_Up);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));

   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options     : Standard.Camera.Lib.Unit_Test.
                     Unit_Test_Program_Options_Type'class
                        renames Standard.Camera.Lib.Unit_Test.
                           Get_Camera_Unit_Test_Constant_Options.all;
      Test_Suite  : constant AUnit.Test_Suites.Access_Test_Suite :=
                     new AUnit.Test_Suites.Test_Suite;
      Load_Test   : constant Configuration_Load_Test_Access :=
                     new Configuration_Load_Test_Type (
                        Brand       => Options.Camera_Options.Brand);
      Tests       : constant Configuration_Tests_Access :=
                     new Configuration_Tests_Type (
                        Brand       => Options.Camera_Options.Brand);

   begin
      Log_In (Debug);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Load_Test);
      Test_Suite.Add_Test (Tests);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

--   ---------------------------------------------------------------
--   overriding
--   procedure Tear_Down (
--      Test                       : in out Configuration_Tests_Type) is
--   ---------------------------------------------------------------
--
--   begin
--      Log_In (Debug);
----    GNOGA_Ada_Lib.Clear_Connection_Data;
--      Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
--      Log_Out (Debug);
--   end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Load (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

--    Connection_Data            : Base.Connection_Data_Type renames
--                                  Base.Connection_Data_Type (
--                                     GNOGA_Ada_Lib.Get_Connection_Data.all);
      Local_Test                 : Configuration_Load_Test_Type renames
                                    Configuration_Load_Test_Type (Test);
--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class renames
--                                     Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
--    State                      : Configuration.Camera.State.State_Type renames
--                                  Connection_Data.State;

   begin
      Log_In (Debug);
      Local_Test.Load_Test_State;
--    Local_Test.Camera.Open (State.Video_Address.all, Local_Test.Port_Number);
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug);
         Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Test_Load;

   ---------------------------------------------------------------
   procedure Test_Update (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       GNOGA_Ada_Lib.Get_Connection_Data.all);
      Configuration_ID           : constant Configuration_ID_Type := 3;
      Expected_Setup             : constant String :=
                                    "expected_updated_test_setup.cfg";
      Local_Test                 : Configuration_Tests_Type renames
                                    Configuration_Tests_Type (Test);
      New_Column                 : constant := 1;
      New_Label                  : constant String := "New Label";
      New_Preset_ID              : constant Video.Lib.Preset_ID_Type :=
                                    Video.Lib.Constructor (5);
      New_Row                    : constant := 2;
      Preset_ID                  : constant Video.Lib.Preset_ID_Type :=
                                    Video.Lib.Constructor (3);
      Update_Setup               : constant String := "updated_setup.cfg";
      Updated_Setup              : Configuration.Camera.Setup.Setup_Type;

   begin
      Log_In (Debug);
--    Local_Test.Setup.Load (Connection_Data.State, Test_Setup);
      if Debug then
         Local_Test.Setup.Get_Configuration (Configuration_ID).Dump ("configuration");
         Local_Test.Setup.Get_Preset (Preset_ID).Dump ("preset");
      end if;
      Updated_Setup := Local_Test.Setup;
      Updated_Setup.Set_Path (Update_Setup);
      Updated_Setup.Update_Configuration (Configuration_ID, New_Label);
      Updated_Setup.Update_Configuration (Configuration_ID, New_Preset_ID);
      Updated_Setup.Update_Preset (New_Preset_ID, New_Row, New_Column);
      Updated_Setup.Update (Connection_Data.State);
      if Debug then
         Updated_Setup.Get_Configuration (Configuration_ID).Dump;
         Updated_Setup.Get_Preset (New_Preset_ID).Dump;
      end if;
      Assert (Updated_Setup.Is_Loaded, "setup not loaded");
      Assert (Updated_Setup.Has_Configuration (Configuration_ID),
         "configuration " &Configuration_ID'img & " deos not exist");
      Assert (Updated_Setup.Has_Preset (New_Preset_ID), "preset " &
         New_Preset_ID'img & " deos not exist");
      declare
         New_Configuration       : constant Configuration_Type'class :=
                                    Local_Test.Setup.Get_Configuration (
                                       Configuration_ID);
         New_Preset              : constant Preset_Type'class :=
                                    Local_Test.Setup.Get_Preset (New_Preset_ID);
      begin
         Assert (New_Configuration.Label.Coerce = New_Label,
            Quote ("configuration label has the wrong value. Got ",
               New_Configuration.Label) &
            Quote (" expected", New_Label));
         Assert (New_Configuration.Preset_ID = New_Preset_ID,
            "wrong preset id. Got" & New_Configuration.Preset_ID.Image &
            " expected" & New_Preset_ID'img &
            " for configuration" & New_Configuration.Configuration_ID'img);
         Assert (New_Preset.Row = New_Row, "got wrong row for preset" &
            New_Preset_ID'img & ". Got" &
            New_Preset.Row'img & " expected" & New_Row'img &
            " for preset" & New_Preset.Preset_ID.Image);
         Assert (New_Preset.Column = New_Column, "got wrong column for preset" &
            New_Preset_ID'img & ". Got" &
            New_Preset.Column'img & " expected" & New_Column'img &
            " for preset" & New_Preset.Preset_ID.Image);

         Assert (Ada_Lib.Directory.Compare_Files (Expected_Setup, Update_Setup),
            "File compare failed");
      end;
      Log_Out (Debug);
   exception


      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug);
         Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Test_Update;

   ---------------------------------------------------------------
   procedure Test_Values (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       GNOGA_Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug, "Number_Configurations" & State.Number_Configurations'img);

      declare
         Expected_Number_Columns    : constant := 3;
         Expected_Number_Rows       : constant := 4;
         Expected_Number_Configurations
                                    : constant := 4;
         Expected_Last_Presets      : constant := 5;
         Local_Test                 : Configuration_Tests_Type renames
                                       Configuration_Tests_Type (Test);

         type Expected_Preset_Type  is array (Standard.Camera.Preset_Range_Type range
                                       Standard.Camera.Preset_Range_Type'first ..
                                       Expected_Last_Presets) of Preset_Type;

         Expected_Presets            : constant Expected_Preset_Type := (
                                       Expected_Preset_Type'(
            0  => (
               Initial_Root_State with
               Column      => 2,
               Row         => 2,
               Preset_ID   => Video.Lib.Constructor (0)),
            1  => (
               Initial_Root_State with
               Column      => 1,
               Row         => 1,
               Preset_ID   => Video.Lib.Constructor (0)),
            3  => (
               Initial_Root_State with
               Column      => 3,
               Row         => 1,
               Preset_ID   => Video.Lib.Constructor (0)),
            5 =>  (
               Initial_Root_State with
               Column      => 2,
               Row         => 1,
               Preset_ID   => Video.Lib.Constructor (0)),
            others => Null_Preset));
         Number_Configurations      : constant Configuration_ID_Type :=
                                       State.Number_Configurations;
         Expected_Configurations    : constant Configurations_Type (
                                       1 .. Number_Configurations) := (
            1 => (
               Initial_Root_State with
               Configuration_ID  => 1,
               Label             => Coerce ("Preset 5"),
               Preset_ID         => Video.Lib.Constructor (5)),
            2 => (
               Initial_Root_State with
               Configuration_ID  => 2,
               Label             => Coerce ("Preset 3"),
               Preset_ID         => Video.Lib.Constructor (3)),
            3 => (
               Initial_Root_State with
               Configuration_ID  => 3,
               Label             => Coerce ("Preset 1"),
               Preset_ID         => Video.Lib.Constructor (1)),
            4 => (
               Initial_Root_State with
               Configuration_ID  => 4,
               Label             => Coerce ("Preset 0"),
               Preset_ID         => Video.Lib.Constructor (0)),
            others => Null_Configuration);

      begin
         declare
            Number_Columns          : constant Column_Type :=
                                       State.Number_Columns;
            Last_Preset             : constant Standard.Camera.Preset_ID_Type :=
                                       Video.Lib.Get_Last_Preset_ID;
            Number_Rows             : constant Row_Type := State.Number_Rows;

         begin
            Log_Here (Debug,
               "Number_Columns" & Number_Columns'img &
               " Number_Configurations" & Number_Configurations'img &
               " Expected_Number_Configurations" & Expected_Number_Configurations'img &
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

            Assert (Last_Preset = Video.Lib.Constructor (Expected_Last_Presets),
               "Last_Preset" & Last_Preset'img &
               " not equal Expected_Last_Presets" &
               Expected_Last_Presets'img);

            Assert (Number_Rows = Expected_Number_Rows,
               "Number_Rows" & Number_Rows'img &
               " not equal Expected_Number_Rows" &
               Expected_Number_Rows'img);

            for Configuration_ID in 1 .. Number_Configurations loop
               Log_Here (Debug, "Configuration_ID" & Configuration_ID'img);
               declare
                  Configuration     : constant Configuration_Type'class :=
                                       Local_Test.Setup.Get_Configuration (Configuration_ID);
                  Expected          : constant Configuration_Type'class :=
                                       Expected_Configurations (Configuration_ID);
               begin
                  if Debug then
                     Configuration.Dump ("Configuration");
                     Expected.Dump ("Expected");
                  end if;

                  Assert (Configuration.Label = Expected.Label,
                     Quote ("got label", Configuration.Label.Coerce) &
                     Quote (" not equal expected label", Expected.Label) &
                     " for configuration" & Configuration_ID'img);

                  Assert (Configuration.Preset_ID = Expected.Preset_ID,
                     "got preset id " & Configuration.Preset_ID.Image &
                     " not equal to " & Expected.Preset_ID.Image &
                     " for configuration" & Configuration_ID'img);
               end;
            end loop;

            for Preset_Number in Standard.Camera.Preset_Range_Type'first ..
                  Last_Preset.ID loop
               declare
                  Expected_Preset_Value
                                    : Preset_Type renames
                                       Expected_Presets (Preset_Number);
               begin
                  if Expected_Preset_Value.Preset_ID.Is_Set then
                     Log_Here (Debug, "Preset number" & Preset_Number'img &
                        " column" & Expected_Preset_Value.Column'img &
                        " row" & Expected_Preset_Value.Row'img);

                     declare
                        Row_Column_Preset_Id
                                          : constant Standard.Camera.Preset_ID_Type :=
                                             Local_Test.Setup.Get_Preset_ID (
                                                Expected_Preset_Value.Row,
                                                Expected_Preset_Value.Column);
                        Preset_By_Preset  : constant Preset_Type'class :=
                                             Local_Test.Setup.Get_Preset (Row_Column_Preset_Id);
                        Preset_ID         : constant Video.Lib.Preset_ID_Type :=
                                             Video.Lib.Constructor (Preset_Number);

                        ---------------------------------------------------------
                        procedure Check_Preset (
                           Preset_Value
                                    : in     Preset_Type'class;
                           How      : in     String) is
                        ---------------------------------------------------------

                           ------------------------------------------------------
                           function Description
                           return String is
                           ------------------------------------------------------

                           begin
                              return " " & How &
                                 " for preset" & Preset_Number'img;
                           end Description;
                           ------------------------------------------------------

                        begin
                           if Debug then
                              Preset_Value.Dump;
                           end if;

                           Assert (Preset_Value.Column =
                              Expected_Preset_Value.Column,
                              "got Column" & Preset_Value.Column'img &
                              " expected column" & Expected_Preset_Value.Column'img &
                              Description);
                           Assert (Preset_Value.Row = Expected_Preset_Value.Row,
                              "got row" & Preset_Value.Row'img &
                              " expected row" & Expected_Preset_Value.Row'img &
                              Description);
                        end Check_Preset;
                        ------------------------------------------------------------

                        Preset_By_Row_Column
                                    : constant Preset_Type'class :=
                                       Local_Test.Setup.Get_Preset (Row_Column_Preset_Id);
                     begin
                        Log_Here (Debug, "preset" & Preset_Number'img &
                           " by row column " & Preset_Image (Preset_By_Row_Column));

                        Check_Preset (Preset_By_Preset, "by preset" & Preset_ID.Image);
                        Check_Preset (Preset_By_Row_Column, "by row column");
                     end;
                  else
                     Log_Here (Debug, "Preset number" & Preset_Number'img & " not set");
                  end if;
               end;
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

end Configuration.Camera.Setup.Unit_Tests;
