with Ada.Exceptions;
--with Ada_Lib.Configuration;
with Ada_Lib.Directory.Compare_Files;
with Ada_Lib.Timer;
with Ada_Lib.GNOGA;
--with Ada_Lib.Options;
with ADA_LIB.Trace; use ADA_LIB.Trace;
--with Ada_Lib.Timer;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Base;
with Camera.Lib.Base;
with Camera.Lib.Unit_Test;
with Configuration.Camera.Setup; use Configuration.Camera;
   use Configuration.Camera.Setup;
--with Events;
with Configuration.Camera.State;
with Gnoga.Gui.Base;
with Gnoga.Gui.View.Card;
with Main;

package body Widgets.Configured.Unit_Test is

   use type Configuration_ID_Type;
   use type Preset_ID_Type;
-- use type Gnoga.Gui.Plugin.Message_Boxes.Message_Box_Result;
   use type Gnoga.Gui.View.Pointer_To_View_Base_Class;

   type Test_Type (
      Brand                      : Camera.Lib.Brand_Type) is new
                                    Camera.Lib.Unit_Test.
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

-- overriding
-- procedure Set_Up (
--    Test                       : in out Test_Type
-- ) with Post => Test.Verify_Set_Up;

   overriding
   procedure Tear_Down (
      Test                       : in out Test_Type
   ) with post => Verify_Torn_Down (Test);

   procedure Test_Select_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Update_Invalid_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Update_Valid_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Update_Preset_Field (
      Setup                      : in     Configuration.Camera.Setup.Setup_Type;
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Preset_ID_Type);

   generic
      type Field_Type            is range <>;

   procedure Check_Field (
      Expected_Value             : in     Field_Type;
      Value                      : in     String;
      Field                      : in     String;
      Check_From                 : in     String;
      From                       : in     String := Here);

   generic

      with procedure Check_Fields (
         Configured_Card            : in     Configured_Card_Type;
         Row_Index                  : in     Row_Index_Type;
         Expected_Configuration_ID  : in     Configuration_ID_Type;
         Expected_Column            : in     Column_Type;
         Expected_Row               : in     Row_Type;
         Expected_Label             : in     String;
         Expected_Image             : in     String;
         Expected_Preset_ID         : in     Preset_ID_Type;
         From                       : in     String := Here);
      type Coordinate_Type is range <>;
      Field                      : Preset_Column_Index_Type;
      with procedure Fire (
         Cell                    : in     Preset_Package.Cell_Class_Access);
      Modified_Configuration_ID  : Configuration_ID_Type;
      Modified_Coordinate_Value_No_Preset
                                 : Integer;
      with procedure Update_Field (
         Cell                    : in     Preset_Package.Cell_Class_Access;
         Value                   : in     Coordinate_Type);

   procedure Test_Update_Invalid_Coordinate (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Check_Fields (
      Configured_Card            : in     Configured_Card_Type;
      Row_Index                  : in     Row_Index_Type;
      Expected_Configuration_ID  : in     Configuration_ID_Type;
      Expected_Column            : in     Column_Type;
      Expected_Row               : in     Row_Type;
      Expected_Label             : in     String;
      Expected_Image             : in     String;
      Expected_Preset_ID         : in     Preset_ID_Type;
      From                       : in     String := Here);

   procedure Column_Fire (
      Cell                       : in     Preset_Package.Cell_Class_Access);

   procedure Row_Fire (
      Cell                       : in     Preset_Package.Cell_Class_Access);

   procedure Update_Column_Field (
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Column_Type);

   procedure Update_Row_Field (
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Row_Type);

   Expected_Setup_Path           : constant String :=
                                    "expected_windows_setup.cfg";
   Suite_Name                    : constant String := "Configured";

   Update_Setup_Path             : constant String :=
                                    "widgets_setup_update.cfg";

   ----------------------------------------------------------------
   procedure Test_Update_Invalid_Coordinate (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Original_Configuration     : constant Configuration_Type'class :=
                                    Local_Test.Setup.Get_Configuration (
                                       Modified_Configuration_ID);
      Current_Card               : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
      Configured_Card            : Configured_Card_Type renames
                                   Configured_Card_Type (Current_Card.all);
   begin
      Log_In (Debug, "test field type " & Field'img &
         " Modified_Configuration_ID" & Modified_Configuration_ID'img &
         " original configuration id" & Original_Configuration.
            Configuration_ID'img &
         " original preset id" & Original_Configuration.Preset_ID'img);
         -- test seting a coordinate that is not used in a preset
         declare
            Cell                 : constant Preset_Package.Cell_Class_Access :=
                                     Preset_Package.Cell_Class_Access (
                                        Configured_Card.Get_Cell (Field,
                                          Modified_Configuration_ID));
            Original_Preset_ID   : Preset_ID_Type renames
                                    Original_Configuration.Preset_ID;
            Original_Preset      : constant Preset_Type'class :=
                                    Local_Test.Setup.Get_Preset (Original_Preset_ID);

         begin
            Log_Here (Debug, "test " & Field'img & " for no preset with " &
               Modified_Coordinate_Value_No_Preset'img);
            -- set the coordinate with no preset defined for the coordinate,column
            -- the preset should be set blank
            Update_Field (Cell, Coordinate_Type (  -- put the test value into the field
               Modified_Coordinate_Value_No_Preset));
            Cell.Dump (Pause_Flag or Debug);
            Pause_On_Flag ("test coordinate value set before fire event");
            Connection_Data.Reset_Update_Event;
            Fire (Cell);
            Log_Here (Debug, "wait for event");
            Connection_Data.Wait_For_Update_Event;

            Cell.Dump (Pause_Flag);
            Pause_On_Flag ("test coordinate value after fire event");
            Check_Fields (Configured_Card,
               Expected_Column=> (case Field is
                                    when Column_Field =>
                                       Column_Type (
                                          Modified_Coordinate_Value_No_Preset),
                                    when Row_Field =>
                                       Original_Preset.Column,
                                    when others =>
                                       Column_Type'first  -- should not happen
                                 ),
               Expected_Configuration_ID
                              => Modified_Configuration_ID,
               Expected_Image => Blank_Preset,
               Expected_Label => Original_Configuration.Label.Coerce,
               Expected_Row   => (case Field is
                                    when Column_Field =>
                                       Original_Preset.Row,
                                    when Row_Field =>
                                       Row_Type (
                                          Modified_Coordinate_Value_No_Preset),
                                    when others =>
                                       Row_Type'first  -- should not happen
                                 ),
               Expected_Preset_ID
                              => Preset_Not_Set,
               Row_Index      => Row_Index_Type (
                                                Modified_Configuration_ID));
         end;   -- test seting a coordinate that is not used in a preset

      Log_Out (Debug);
   end Test_Update_Invalid_Coordinate;

   procedure Test_Update_Invalid_Column is new Test_Update_Invalid_Coordinate (
      Check_Fields                        => Check_Fields,
      Coordinate_Type                     => Column_Type,
      Field                               => Column_Field,
      Fire                                => Column_Fire,
      Modified_Configuration_ID           => 2,
      Modified_Coordinate_Value_No_Preset => 3,
      Update_Field                        => Update_Column_Field);

   procedure Test_Update_Invalid_Row is new Test_Update_Invalid_Coordinate (
      Check_Fields                        => Check_Fields,
      Coordinate_Type                     => Row_Type,
      Field                               => Row_Field,
      Fire                                => Row_Fire,
      Modified_Configuration_ID           => 2,
      Modified_Coordinate_Value_No_Preset => 2,
      Update_Field                        => Update_Row_Field);

   generic

      with procedure Check_Fields (
         Configured_Card            : in     Configured_Card_Type;
         Row_Index                  : in     Row_Index_Type;
         Expected_Configuration_ID  : in     Configuration_ID_Type;
         Expected_Column            : in     Column_Type;
         Expected_Row               : in     Row_Type;
         Expected_Label             : in     String;
         Expected_Image             : in     String;
         Expected_Preset_ID         : in     Preset_ID_Type;
         From                       : in     String := Here);
      type Coordinate_Type is range <>;
      Expected_Preset_ID         : Preset_ID_Type;
      Field                      : Preset_Column_Index_Type;
      with procedure Fire (
         Cell                    : in     Preset_Package.Cell_Class_Access);
      Modified_Configuration_ID  : Configuration_ID_Type;
      Modified_Field_Value_With_Preset
                                 : Coordinate_Type;
      with procedure Update_Field (
         Cell                    : in     Preset_Package.Cell_Class_Access;
         Value                   : in     Coordinate_Type);

   procedure Test_Update_Valid_Coordinate (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   ----------------------------------------------------------------
   procedure Test_Update_Valid_Coordinate (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Original_Configuration     : constant Configuration_Type'class :=
                                    Local_Test.Setup.Get_Configuration (
                                       Modified_Configuration_ID);
      Current_Card               : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
      Configured_Card            : Configured_Card_Type renames
                                   Configured_Card_Type (Current_Card.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug, "field " & Field'img &
         " Modified_Configuration_ID" & Modified_Configuration_ID'img &
         " original configuration id" & Original_Configuration.
            Configuration_ID'img &
         " original preset id" & Original_Configuration.Preset_ID'img &
         " Modified_Field_Value_With_Preset" &
            Modified_Field_Value_With_Preset'img &
         " Expected_Preset_ID" & Expected_Preset_ID'img);
         -- test seting a row/column that is valid for a different preset
         -- preset field should be updated
         declare
--          New_Preset           : constant Preset_Type :=
--                                  Local_Test.Setup.Get_Preset (Expected_Preset_ID);
            Cell                 : constant Preset_Package.Cell_Class_Access :=
                                     Preset_Package.Cell_Class_Access (
                                        Configured_Card.Get_Cell (Field,
                                           Modified_Configuration_ID));
            Original_Preset_ID   : Preset_ID_Type renames
                                    Original_Configuration.Preset_ID;
            Original_Preset      : constant Preset_Type'class :=
                                    Local_Test.Setup.Get_Preset (Original_Preset_ID);
         begin
            Log_Here (Debug, "test " & Field'img & " for valid preset");
            Connection_Data.Reset_Update_Event;
            Update_Field (Cell, Modified_Field_Value_With_Preset);

            Cell.Dump (Pause_Flag or Debug);
            Pause_On_Flag ("test " & Field'img & " value set before fire event");
            Fire (Cell);
            Log_Here (Debug, "wait for event");
            Connection_Data.Wait_For_Update_Event;
            delay 0.5;  -- let web page update
            Cell.Dump (Pause_Flag);
            Pause_On_Flag ("test " & Field'img & " value after fire event");
            Check_Fields (Configured_Card,
               Expected_Column=> (case Field is
                                    when Column_Field =>
                                       Column_Type (
                                          Modified_Field_Value_With_Preset),
                                    when Row_Field =>
                                       Original_Preset.Column,
                                    when others =>
                                       Column_Type'first  -- should not happen
                                 ),
               Expected_Configuration_ID
                              => Modified_Configuration_ID,
               Expected_Image => State.Image_Path (
                                    Column => (case Field is
                                          when Column_Field =>
                                             Column_Type (
                                                Modified_Field_Value_With_Preset),
                                          when Row_Field =>
                                             Original_Preset.Column,
                                          when others =>
                                             Column_Type'first  -- should not happen
                                       ),
                                    Row   => (case Field is
                                          when Column_Field =>
                                             Original_Preset.Row,
                                          when Row_Field =>
                                             Row_Type (
                                                Modified_Field_Value_With_Preset),
                                          when others =>
                                             Row_Type'first  -- should not happen
                                       ),
                                    Add_Prefix  => True),
               Expected_Label => Original_Configuration.Label.Coerce,
               Expected_Row   => (case Field is
                                    when Column_Field =>
                                       Original_Preset.Row,
                                    when Row_Field =>
                                       Row_Type (
                                          Modified_Field_Value_With_Preset),
                                    when others =>
                                       Row_Type'first  -- should not happen
                                 ),
               Expected_Preset_ID
                              => Expected_Preset_ID,
               Row_Index      => Row_Index_Type (Modified_Configuration_ID));
        end;   -- test seting a row/column that is not used in a preset
      Log_Out (Debug);
   end Test_Update_Valid_Coordinate;

   procedure Test_Update_Valid_Column is new Test_Update_Valid_Coordinate (
      Check_Fields                        => Check_Fields,
      Coordinate_Type                     => Column_Type,
      Expected_Preset_ID                  => 1,
      Field                               => Column_Field,
      Fire                                => Column_Fire,
      Modified_Configuration_ID           => 1,
      Modified_Field_Value_With_Preset    => 2,
      Update_Field                        => Update_Column_Field);

   procedure Test_Update_Valid_Row is new Test_Update_Valid_Coordinate (
      Check_Fields                        => Check_Fields,
      Coordinate_Type                     => Row_Type,
      Expected_Preset_ID                  => 3,
      Field                               => Row_Field,
      Fire                                => Row_Fire,
      Modified_Configuration_ID           => 2,
      Modified_Field_Value_With_Preset    => 3,
      Update_Field                        => Update_Row_Field);

   ---------------------------------------------------------------
   procedure Check_Field (
      Expected_Value             : in     Field_Type;
      Value                      : in     String;
      Field                      : in     String;
      Check_From                 : in     String;
      From                       : in     String := Here) is
   ---------------------------------------------------------------

      Field_Value                : constant Field_Type :=
                                    (if Value'length = 0 then
                                       Field_Type'last
                                    else
                                       Field_Type'value (Value));

   begin
      Log_Here (Debug, "expected value " & Expected_Value'img &
         " field_Value" & Field_Value'img &
         Quote (" value", Value) & Quote (" field", Field) &
         " from " & From);

      Assert (Expected_Value = Field_Value,
         "Invalid value for " & Field &
         Quote (" got", Value) & " (" & Field_Value'img & ")" &
         " expected" & Expected_Value'img & " check from " & Check_From &
         " from " & From);
   end Check_Field;
   ---------------------------------------------------------------

   procedure Check_Column is new Check_Field (Column_Type);
   procedure Check_Preset is new Check_Field (Preset_ID_Type);
   procedure Check_Row is new Check_Field (Row_Type);

   type Test_Case_Type           is (Accept_Form, Cancel_Form, Not_Set,
                                    Update_Configuration, Update_Label);

   type Button_Push_Event_Type   is new Ada_Lib.Timer.Event_Type with record
      Test_Case                  : Test_Case_Type := Not_Set;
   end record;

   overriding
   procedure Callback (
      Event                   : in out Button_Push_Event_Type);

   procedure Test_Accept_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Cancel_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Create_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Update_Label (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Update_Valid_Column (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);
--
-- procedure Test_Update_Valid_Row (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- Description                   : aliased String := "button push callback event";
-- Setup_Test_Path               : constant String := "configured_window_setup.cfg";
-- State_Test_Path               : constant String := "configured_window_state.cfg";
   Updated_Column                : constant Preset_Column_Index_Type :=
                                       Label_Field;
   Updated_Label_Content         : constant String := "updated label content";
   Updated_Row                   : constant Preset_Row_Index_Type := 2;

   ---------------------------------------------------------------
   overriding
   procedure Callback (
      Event                   : in out Button_Push_Event_Type) is
   ---------------------------------------------------------------

      Connection_Data         : constant Standard.Base.Connection_Data_Access :=
                                 Standard.Base.Connection_Data_Access (
                                    Ada_Lib.GNOGA.Get_Connection_Data);
      Configured_Card         : constant Configured_Card_Access :=
                                 Connection_Data.Get_Configured_Card;
   begin
      Log_In (Debug, "test case " & Event.Test_Case'img);
      case Event.Test_Case is

         when Accept_Form =>
            Configured_Card.Get_Accept_Button.Fire_On_Click;

         when Cancel_Form =>
            Configured_Card.Get_Cancel_Button.Fire_On_Click;

         when Not_Set =>
            Ada_Lib.Unit_Test.Set_Failed;
            raise Failed with "Parameter.Test_Case not set";

         when Update_Configuration =>
            Log_Here (Debug);
            Connection_Data.Close_Message_Box;

         when Update_Label =>
            declare
               Cell              : constant Generic_Cell_Package.Cell_Class_Access :=
                                       Configured_Card.Get_Cell (
                                          Updated_Column, Updated_Row);
               Configured_Cell   : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (Cell);
            begin
               Log_Here (Debug, Quote ("cell id", Cell.ID) &
                  Quote ("label id", Configured_Cell.Label.ID));
--             Configured_Cell.Update_Label (Updated_Label_Content);
               Configured_Cell.Label.Value (Updated_Label_Content);
               Configured_Cell.Label.Fire_On_Focus_Out;
               Pause_On_Flag ("field updated");
            end;

      end case;

--    Button_Push_Event.Set_Event;
      Log_Out (Debug);
   end Callback;

   ---------------------------------------------------------------
   procedure Check_Fields (
      Configured_Card            : in     Configured_Card_Type;
      Row_Index                  : in     Row_Index_Type;
      Expected_Configuration_ID  : in     Configuration_ID_Type;
      Expected_Column            : in     Column_Type;
      Expected_Row               : in     Row_Type;
      Expected_Label             : in     String;
      Expected_Image             : in     String;
      Expected_Preset_ID         : in     Preset_ID_Type;
      From                       : in     String := Here) is
   ---------------------------------------------------------------

      Message                    : constant String :=
         " row index" & Row_Index'img &
         " Expected_Configuration_ID " & Expected_Configuration_ID'img &
         " Expected_Column " & Expected_Column'img &
         " Expected_Row " & Expected_Row'img &
         " Expected_Label " & Expected_Label &
         " Expected_Image " & Expected_Image &
         " Expected_Preset_ID " & Expected_Preset_ID'img &
         " from " & From;

   begin
      Log_In (Debug, Message);

      for Column in Preset_Column_Index_Type'range loop
         Log_Here (Debug, "column " & Column'img);

         declare
            Cell                 : constant Preset_Package.
                                    Cell_Class_Access := Preset_Package.
                                          Cell_Class_Access (
                                       Configured_Card.Get_Cell (Column,
                                          Configuration_ID_Type (Row_Index)));
         begin
            case Column is
               when Column_Field =>
                  declare
                     Value          : constant String :=
                                       Cell.Column_Coordinate.Value;
                  begin
                     Check_Column (Expected_Column, Value, "column", From);
                  end;

               when Image_Field =>
                  Assert (Expected_Image = Cell.Image_Div.Path.Coerce,
                     Quote ("Invalid path ", Cell.Image_Div.Path) &
                     Quote (" expected", Expected_Image) &
                     " from " & From);

               when Preset_Field =>
                  declare
                     Value          : constant String :=
                                       Cell.Preset_ID_Field.Value;
                  begin
                     Check_Preset (Expected_Preset_ID, Value, "preset", From);
                  end;

               when Row_Field =>
                  declare
                     Value          : constant String :=
                                       Cell.Row_Coordinate.Value;
                  begin
                     Check_Row (Expected_Row, Value, "row", From);
                  end;

               when others =>
                  Null;          -- not checked

            end case;

         exception

            when Fault: others =>
                  Trace_Message_Exception (Debug, Fault, "column " &
                     Column'img & Message);
                  Assert (False, "exception in " & Here & Message);

         end;
      end loop;
      Log_Out (Debug);

   exception

      when Fault: others =>
            Trace_Message_Exception (Debug, Fault, Message);
            Assert (False, "exception in " & Here & Message);

   end Check_Fields;

   ---------------------------------------------------------------
   procedure Column_Fire (
      Cell                       : in     Preset_Package.Cell_Class_Access) is
   ---------------------------------------------------------------

   begin
      Cell.Column_Coordinate.Fire_On_Focus_Out;
   end Column_Fire;

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
         Routine        => Test_Create_Configured'access,
         Routine_Name   => AUnit.Format ("Test_Create_Configured")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Accept_Configured'access,
         Routine_Name   => AUnit.Format ("Test_Accept_Configured")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Cancel_Configured'access,
         Routine_Name   => AUnit.Format ("Test_Cancel_Configured")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Select_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Select_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Label'access,
         Routine_Name   => AUnit.Format ("Test_Update_Label")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Invalid_Column'access,
         Routine_Name   => AUnit.Format ("Test_Update_Invalid_Column")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Invalid_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Update_Invalid_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Invalid_Row'access,
         Routine_Name   => AUnit.Format ("Test_Update_Invalid_Row")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Valid_Column'access,
         Routine_Name   => AUnit.Format ("Test_Update_Valid_Column")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Valid_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Update_Valid_Preset")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Valid_Row'access,
         Routine_Name   => AUnit.Format ("Test_Update_Valid_Row")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   procedure Row_Fire (
      Cell                       : in     Preset_Package.Cell_Class_Access) is
   ---------------------------------------------------------------

   begin
      Cell.Row_Coordinate.Fire_On_Focus_Out;
   end Row_Fire;

--   ---------------------------------------------------------------
--   overriding
--   procedure Set_Up (
--      Test                       : in out Test_Type) is
--   ---------------------------------------------------------------
--
--      Options                    : Standard.Camera.Lib.Unit_Test.
--                                    Unit_Test_Program_Options_Type'class
--                                       renames Standard.Camera.Lib.Unit_Test.
--                                          Options.all;
--      Connection_Data            : Base.Connection_Data_Type renames
--                                    Base.Connection_Data_Type (
--                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
--      State                      : Configuration.Camera.State.State_Type renames
--                                    Connection_Data.State;
--   begin
--      Log_In (Debug or Trace_Set_Up, "test tag " & Tag_Name (Test_Type'class (Test)'tag));
----    Ada_Lib.GNOGA.Set_Connection_Data ( -- moved to parent
----       Ada_Lib.GNOGA.Connection_Data_Class_Access (Connection_Data));
----    State.Load (
----       Options.Camera_Options.Location, State_Test_Path); -- need to load state 1st
----    Test.Setup.Load (State, Setup_Test_Path);
--      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Set_Up;
----    Button_Push_Event.Reset_Event;
--      Log_Out (Debug or Trace_Set_Up);
--
--   exception
--      when Fault: Failed =>
--         Test.Set_Up_Message_Exception (Fault, "could not load configuration" &
--            Quote (" Setup_Path", Setup_Test_Path) &
--            Quote (" State_Path", State_Test_Path));
--         raise;
--
--      when Fault: others =>
--         Trace_Exception (Debug, Fault);
--         Assert (False, "exception message " & Ada.Exceptions.Exception_Message (Fault));
--
--   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

--    Options                    : Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type'class
--                                  renames Camera.Lib.Unit_Test.Options.all;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type (
                                    Camera.Lib.PTZ_Optics_Camera);

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

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug);
      Ada_Lib.GNOGA.Clear_Connection_Data;

      if Test.Setup.Loaded then
         Test.Setup.Unload (State, Save_Changes => False);
      end if;
      Camera.Lib.Unit_Test.Camera_Window_Test_Type (Test).Tear_Down;
      Log_Out (Debug);
   end Tear_Down;

   ----------------------------------------------------------------
   procedure Test_Accept_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames
--                                 Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Tabs                       : constant Gnoga.Gui.View.Card.Pointer_To_Tab_Class :=
                                    Connection_Data.Get_Tabs;
   begin
      Log_In (Debug);
      Tabs.Select_Tab (Widget_Name);
      Pause_On_Flag ("configured tab displayed", Here, Debug);

      declare
         Current_Card            : constant Gnoga.Gui.View.Pointer_To_View_Base_Class :=
                                    Cards.Card (Widget_Name);
      begin
         Assert (Current_Card /= Null, "configured card not found");
         Assert (Current_Card.Visible, "configured card not visible");
      end;

      declare
         Event                   : Button_Push_Event_Type ;

      begin
         Event.Initialize (
            Wait           => 0.25,
            Description    => "accept button");
         Event.Test_Case := Accept_Form;

--       Event.Set (Offset => 0.25);
         delay 0.5;     -- wait for button to be pushed
      end;
      Log_Out (Debug);

   exception
      when Fault: AUnit.Assertions.Assertion_Error =>
         Trace_Exception (Debug, Fault);

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, Exception_Info (Fault) & " in " & Who);

   end Test_Accept_Configured;

   ----------------------------------------------------------------
   procedure Test_Cancel_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames
--                                 Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Tabs                       : constant Gnoga.Gui.View.Card.Pointer_To_Tab_Class :=
                                    Connection_Data.Get_Tabs;
   begin
      Log_In (Debug);
      Tabs.Select_Tab (Widget_Name);
      Pause_On_Flag ("configured tab displayed", Here, Debug);

      declare
         Current_Card            : constant Gnoga.Gui.View.Pointer_To_View_Base_Class :=
                                    Cards.Card (Widget_Name);
      begin
         Assert (Current_Card /= Null, "configured card not found");
         Assert (Current_Card.Visible, "configured card not visible");
      end;

      declare
         Event                   : Button_Push_Event_Type;

      begin
         Event.Initialize (
            Wait           => 0.25,
            Description    => "cancel button");
         Event.Test_Case := Cancel_Form;
--       Button_Push_Event.Wait_For_Event;
         delay 0.5;
      end;

--    Assert (not Parameter.Exception_Occured, "exception in button handler");
      Log_Out (Debug);

   end Test_Cancel_Configured;

   ----------------------------------------------------------------
   procedure Test_Create_Configured (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
       Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug);

      declare
         Current_Card            : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
         Configured_Card         : Configured_Card_Type renames
                                    Configured_Card_Type (Current_Card.all);
      begin
         -- test configurations
         for Configuration_Index in Configuration_ID_Type'first ..
               State.Number_Configurations loop
            declare
               Configuration     : Configuration_Type
                                    renames Global_Camera_Setup.Configurations (
                                       Configuration_Index);
               Expected_Preset_ID: constant Preset_ID_Type :=
                                    Local_Test.Setup.Get_Preset_ID (
                                       Configuration_Index);
               Expected_Label : Ada_Lib.Strings.Unlimited.String_Type renames
                                          Configuration.Label;
            begin
               for Column_Index in Preset_Column_Index_Type'range loop
                  if Column_Index /= Control_Field then
                     -- control field only for 1s row configuration table
                     Log_Here (Debug, "configuration" & Configuration_Index'img &
                        " column " & Column_Index'img);
                     declare
                        Cell           : Preset_Package.Cell_Type'class renames
                                          Preset_Package.Cell_Type'class (
                                             Configured_Card.Get_Cell (Column_Index,
                                                Configuration_Index).all);
                        Expected_Preset: constant
                                             Preset_Type'class :=
                                          Local_Test.Setup.Get_Preset (
                                             Expected_Preset_ID);
                        Expected_Column: Column_Type renames
                                          Expected_Preset.Column;
                        Expected_Row   : Row_Type renames
                                          Expected_Preset.Row;
                     begin
                        Cell.Dump (Debug);
                        Assert (Cell.Configuration_ID = Configuration_Index,
                           "expected configuration id (" &
                              Configuration_Index'img &
                           ") does not match configuration id (" &
                              Cell.Configuration_ID'img & ")");

                        case Cell.Column is
                           when Column_Field =>
                              declare
                                 Field_Contents
                                       : constant String :=
                                          Cell.Column_Coordinate.Value;
                              begin
                                 Assert (Cell.Column_Number = Expected_Column,
                                    "expected column (" & Expected_Column'img &
                                    ") does not match column (" &
                                       Cell.Column_Number'img & ")");

                                 if Expected_Column = Column_Not_Set then
                                    Assert (Field_Contents'length = 0,
                                       "expected column coordinage is not blank," &
                                       " got" & Field_Contents);
                                 else
                                    Assert (Column_Type'value (Field_Contents) =
                                       Expected_Column,
                                       "expected column (" & Expected_Column'img &
                                       ") does not match column (" &
                                          Field_Contents & ")");
                                 end if;
                              end;

                           when Label_Field =>
                              Assert (Cell.Label.Value = Expected_Label.Coerce,
                                 Quote ("expected label (", Expected_Label) &
                                 Quote (") does not match label (",
                                    Cell.Label.Value) &
                                 ")");

                           when Preset_Field =>
                              Assert (Cell.Preset_ID = Expected_Preset_ID,
                                 "expected preset id (" & Expected_Preset_ID'img &
                                 ") does not match preset id (" &
                                    Cell.Preset_ID'img & ")");

                           when Row_Field =>
                              declare
                                 Field_Contents
                                       : constant String :=
                                          Cell.Row_Coordinate.Value;
                              begin
                                 Assert (Cell.Row_Number = Expected_Row,
                                    "expected Row (" & Expected_Row'img &
                                    ") does not match Row (" &
                                       Cell.Row_Number'img & ")");

                                 if Expected_Row = Row_Not_Set then
                                    Assert (Field_Contents'length = 0,
                                       "expected Row coordinage is not blank," &
                                       " got" & Field_Contents);
                                 else
                                    Assert (Row_Type'value (Field_Contents) =
                                       Expected_Row,
                                       "expected Row (" & Expected_Row'img &
                                       ") does not match Row (" &
                                          Field_Contents & ")");
                                 end if;
                              end;

                           when others =>
                              null; -- no checks

                        end case;
                     end;
                  end if;
               end loop;
            end;
         end loop;
      end;

      Log_Out (Debug);

   exception
      when Fault: AUnit.Assertions.Assertion_Error =>
         Trace_Exception (Debug, Fault);

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, Exception_Info (Fault) & " in " & Who);

   end Test_Create_Configured;

   ----------------------------------------------------------------
   procedure Test_Select_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------

--    Local_Test                 : Test_Type'class renames
--                                 Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Modified_Configuration_ID  : constant := 2;
--    Modified_Preset_Value_No_Preset
--                               : constant := 2;
--    Original_Configuration     : constant Configuration_Type'class :=
--                                  Local_Test.Setup.Get_Configuration (
--                                     Modified_Configuration_ID);
      Current_Card               : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
      Configured_Card            : Configured_Card_Type renames
                                   Configured_Card_Type (Current_Card.all);
   begin
      Log_In (Debug);
         Connection_Data.Camera.Set_Preset (Camera.Lib.Base.Power_On_Preset);
         declare
            Cell                 : constant Preset_Package.Cell_Class_Access :=
                                     Preset_Package.Cell_Class_Access (
                                        Configured_Card.Get_Cell (Preset_Field,
                                          Modified_Configuration_ID));
            Preset_ID_Field      : Gnoga.Gui.Element.Form.Number_Type renames
                                    Cell.Preset_ID_Field;
         begin
            Log_Here (Debug, "test preset for no preset");
            -- set the preset with no preset defined for the preset,column
            -- the preset should be set blank
            Preset_ID_Field.Fire_On_Mouse_Click (
               Gnoga.Gui.Base.Mouse_Event_Record'(
                  Message       => Gnoga.Gui.Base.Unknown,
                  X             => 0,
                  Y             => 0,
                  Screen_X      => 0,
                  Screen_Y      => 0,
                  Left_Button   => True,
                  Middle_Button => False,
                  Right_Button  => False,
                  Alt           => False,
                  Control       => False,
                  Shift         => False,
                  Meta          => False));
         end;

      Log_Out (Debug);
   end Test_Select_Preset;

   ----------------------------------------------------------------
   procedure Test_Update_Invalid_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Modified_Configuration_ID  : constant := 2;
      Modified_Preset_Value_No_Preset
                                 : constant := 2;
      Original_Configuration     : constant Configuration_Type'class :=
                                    Local_Test.Setup.Get_Configuration (
                                       Modified_Configuration_ID);
      Current_Card               : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
      Configured_Card            : Configured_Card_Type renames
                                   Configured_Card_Type (Current_Card.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug, "Modified_Configuration_ID" & Modified_Configuration_ID'img &
         " original configuration id" & Original_Configuration.
            Configuration_ID'img &
         " original preset id" & Original_Configuration.Preset_ID'img);
         -- test seting a preset that is not used in a preset
         declare
            Cell                 : constant Preset_Package.Cell_Class_Access :=
                                     Preset_Package.Cell_Class_Access (
                                        Configured_Card.Get_Cell (Preset_Field,
                                          Modified_Configuration_ID));
            Original_Preset_ID   : Preset_ID_Type renames
                                    Original_Configuration.Preset_ID;
            Original_Preset      : constant Preset_Type'class :=
                                    Local_Test.Setup.Get_Preset (Original_Preset_ID);

         begin
            Log_Here (Debug, "test preset for no preset");
            -- set the preset with no preset defined for the preset,column
            -- the preset should be set blank
            Update_Preset_Field (Local_Test.Setup, Cell, Modified_Preset_Value_No_Preset);
            Cell.Dump (Pause_Flag or Debug);
            Pause_On_Flag ("test preset value set before fire event");
            Connection_Data.Reset_Update_Event;
            Cell.Preset_ID_Field.Fire_On_Focus_Out;
            Log_Here (Debug, "wait for event");
            Connection_Data.Wait_For_Update_Event;

            Cell.Dump (Pause_Flag);
            Pause_On_Flag ("test preset value after fire event");
            Check_Fields (Configured_Card,
               Expected_Column=> Original_Preset.Column,
               Expected_Configuration_ID
                              => Modified_Configuration_ID,
               Expected_Image => State.Image_Path (
                                    Original_Preset.Row,
                                    Original_Preset.Column,
                                    Add_Prefix => True),
               Expected_Label => Original_Configuration.Label.Coerce,
               Expected_Row   => Original_Preset.Row,
               Expected_Preset_ID
                              => Column_Not_Set,
               Row_Index      => Row_Index_Type (
                                                Modified_Configuration_ID));
         end;   -- test seting a preset that is not used in a preset

      Log_Out (Debug);
   end Test_Update_Invalid_Preset;

   ----------------------------------------------------------------
   procedure Test_Update_Label (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      Tabs                       : constant Gnoga.Gui.View.Card.Pointer_To_Tab_Class :=
                                    Connection_Data.Get_Tabs;
   begin
      Log_In (Debug);
      Tabs.Select_Tab (Widget_Name);
      Pause_On_Flag ("configured tab displayed", Here, Debug);

      declare
         Current_Card            : constant Gnoga.Gui.View.Pointer_To_View_Base_Class :=
                                    Cards.Card (Widget_Name);
      begin
         Assert (Current_Card /= Null, "configured card not found");
         Assert (Current_Card.Visible, "configured card not visible");
      end;

      declare
         Event                   : Button_Push_Event_Type;

      begin
         Event.Initialize (
            Wait           => 0.25,
            Description    => "update button");
         Event.Test_Case := Update_Label;
         delay 0.5;     -- wait for focus to leave label
      end;
      declare
         Configured_Card         : constant Configured_Card_Access :=
                                    Connection_Data.Get_Configured_Card;
         Cell                    : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Configured_Card.Get_Cell (
                                         Updated_Column, Updated_Row));
         Label                   : constant String := Cell.Label.Value;

      begin
         Log_Here (Debug, Quote ("update label", Label));
         Assert (Label = Updated_Label_Content,
            Quote ("wrong label value", Label) &
            Quote (" expected", Updated_Label_Content));
      end;

      Local_Test.Setup.Set_Path (Update_Setup_Path);
      Local_Test.Setup.Unload (State, Save_Changes => True);

      Assert (Ada_Lib.Directory.Compare_Files (Update_Setup_Path,
         Expected_Setup_Path), "files did not compare");
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug);
         Assert (False, Ada.Exceptions.Exception_Message (Fault));

   end Test_Update_Label;

   ----------------------------------------------------------------
   procedure Test_Update_Valid_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ----------------------------------------------------------------

      Local_Test                 : Test_Type'class renames
                                   Test_Type'class (Test);
      Connection_Data            : constant Base.Connection_Data_Access :=
                                    Base.Connection_Data_Access (
                                       Ada_Lib.GNOGA.Get_Connection_Data);
      Cards                      : constant Main.Cards_Access_Type :=
                                    Connection_Data.Get_Cards;
      Expected_Preset_ID         : constant := 5;
      Expected_Preset            : constant Preset_Type'class :=
                                    Local_Test.Setup.Get_Preset (Expected_Preset_ID);
      Modified_Configuration_ID  : constant := 3;
      Modified_Field_Value_With_Preset
                                 : constant := 5;
      Original_Configuration     : constant Configuration_Type'class :=
                                    Local_Test.Setup.Get_Configuration (
                                       Modified_Configuration_ID);
      Current_Card               : constant Gnoga.Gui.View.
                                    Pointer_To_View_Base_Class :=
                                       Cards.Card (Widget_Name);
      Configured_Card            : Configured_Card_Type renames
                                   Configured_Card_Type (Current_Card.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
   begin
      Log_In (Debug, "preset field " &
         " Modified_Configuration_ID" & Modified_Configuration_ID'img &
         " original configuration id" & Original_Configuration.
            Configuration_ID'img &
         " original preset id" & Original_Configuration.Preset_ID'img &
         " Modified_Field_Value_With_Preset" &
            Modified_Field_Value_With_Preset'img &
         " Expected_Preset_ID" & Expected_Preset_ID'img);
         -- test seting a row/column that is valid for a different preset
         -- preset field should be updated
         declare
--          New_Preset           : constant Preset_Type :=
--                                  Local_Test.Setup.Get_Preset (Expected_Preset_ID);
            Cell                 : constant Preset_Package.Cell_Class_Access :=
                                     Preset_Package.Cell_Class_Access (
                                        Configured_Card.Get_Cell (Preset_Field,
                                           Modified_Configuration_ID));
--          Original_Preset_ID   : Preset_ID_Type renames
--                                  Original_Configuration.Preset_ID;
--          Original_Preset      : constant Preset_Type :=
--                                  Local_Test.Setup.Get_Preset (Original_Preset_ID);
         begin
            Log_Here (Debug, "test preset field");
            Connection_Data.Reset_Update_Event;
            Update_Preset_Field (Local_Test.Setup, Cell,
               Modified_Field_Value_With_Preset);

            Cell.Dump (Pause_Flag or Debug);
            Pause_On_Flag ("test preset Field value set before fire event");
            Cell.Preset_ID_Field.Fire_On_Focus_Out;
            Log_Here (Debug, "wait for event");
            Connection_Data.Wait_For_Update_Event;
            delay 0.5;  -- let web page update
            Cell.Dump (Pause_Flag);
            Pause_On_Flag ("test preset Field value after fire event");
            Check_Fields (Configured_Card,
               Expected_Column=> Expected_Preset.Column,
               Expected_Configuration_ID
                              => Modified_Configuration_ID,
               Expected_Image => State.Image_Path (Expected_Preset.Row,
                                    Expected_Preset.Column,
                                    Add_Prefix => True),
               Expected_Label => Original_Configuration.Label.Coerce,
               Expected_Row   => Expected_Preset.Row,
               Expected_Preset_ID
                              => Expected_Preset_ID,
               Row_Index      => Row_Index_Type (
                                    Modified_Configuration_ID));
        end;   -- test seting a row/column that is not used in a preset
      Log_Out (Debug);
   end Test_Update_Valid_Preset;

   ----------------------------------------------------------------
   procedure Update_Column_Field (
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Column_Type) is
   ----------------------------------------------------------------

   begin
      Cell.Column_Coordinate.Value (Integer (Value));
   end Update_Column_Field;

   ----------------------------------------------------------------
   procedure Update_Preset_Field (
      Setup                      : in     Configuration.Camera.Setup.Setup_Type;
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Preset_ID_Type) is
   ----------------------------------------------------------------

   begin
      if Setup.Has_Preset (Value) then
         Cell.Preset_ID_Field.Value (Integer (Value));
         Cell.Preset_Set := True;
      else
         Cell.Preset_ID_Field.Value ("");
         Cell.Preset_Set := False;
      end if;

      Cell.Preset_ID := Value;
   end Update_Preset_Field;

   ----------------------------------------------------------------
   procedure Update_Row_Field (
      Cell                       : in     Preset_Package.Cell_Class_Access;
      Value                      : in     Row_Type) is
   ----------------------------------------------------------------

   begin
      Cell.Row_Coordinate.Value (Integer (Value));
   end Update_Row_Field;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;
--Debug := True;
   Log_Here (Elaborate or Trace_Options);
end Widgets.Configured.Unit_Test;

