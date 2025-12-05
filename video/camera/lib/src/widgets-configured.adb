with Ada.Tags;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.States; use Camera.States;
with Configuration.Camera.Setup;
-- use Configuration.Camera;
-- use Configuration.Camera.Setup;
with Configuration.Camera.State;
with GNOGA_Ada_Lib;
with Video.Lib;

package body Widgets.Configured is

   use type Ada.Tags.Tag;
-- use type Camera.Preset_Range_Type;

      type Update_Parameter_Type    is new Generic_Cell_Package.
                                       Update_Parameter_Type with record
         Column_Number           : Configuration.Column_Type;
         Column_Coordinate       : Integer; -- Ada_Lib.Strings.Unlimited.String_Type;
         Preset_ID               : Camera.Preset_ID_Type;
         Row_Coordinate          : Integer; -- Ada_Lib.Strings.Unlimited.String_Type;
         Row_Number              : Configuration.Row_Type;
      end record;

   Column_Labels                 : aliased Column_Labels_Type := (
      new String'("Configuration"),
      new String'("Label"),
      new String'("Preset"),
      new String'("Row"),
      new String'("Column"),
      new String'("Image"),
      new String'("Control"));

   generic
      type Field_Type            is range <>;
      type State_Type ( <> )     is tagged private;

      with function Last_Value (
         State                   : in     State_Type
      ) return Field_Type;

   package Field_Package is

      function Valid (
         State                      : in     State_Type;
         Value                      : in     Integer
      ) return Boolean;

      procedure Update (
         State                      : in     State_Type;
         Value_Field                : in out Field_Type;
         Form_Field                 : in out Gnoga.Gui.Element.Form.Number_Type;
         Value                      : in     Field_Type;
         From                       : in     String := Ada_Lib.Trace.Here);

   end Field_Package;

   package body Field_Package is

      ---------------------------------------------------------------
      function Valid (
         State                      : in     State_Type;
         Value                      : in     Integer
      ) return Boolean is
      ---------------------------------------------------------------

      begin
         return Value <= Integer (Last_Value (State)) and then
                Value >= Integer (Field_Type'first);
      end Valid;

      ---------------------------------------------------------------
      procedure Update (
         State                      : in     State_Type;
         Value_Field                : in out Field_Type;
         Form_Field                 : in out Gnoga.Gui.Element.Form.Number_Type;
         Value                      : in     Field_Type;
         From                       : in     String := Ada_Lib.Trace.Here) is
      ---------------------------------------------------------------

      begin
         Log_In (Debug, "value " & Value'img &
            " max " & Last_Value (State)'img &
            " was" & Value_Field'img & Quote (" field", Form_Field.Value) &
            " from " & From);
         Value_Field := Value;
         if Value > Last_Value (State) then
            Form_Field.Value ("");
         else
            Form_Field.Value (Integer (Value));
         end if;
         Log_Out (Debug, "new" & Value_Field'img &
            Quote (" field", Form_Field.Value));
      end Update;

   end Field_Package;
   ---------------------------------------------------------------

   package Column_Package is new Field_Package (Configuration.Column_Type,
      Configuration.Camera.State.State_Type,
      Configuration.Camera.State.Get_Number_Columns);
   package Preset_ID_Package is new Field_Package (Natural,
      Configuration.Camera.State.State_Type,
      Configuration.Camera.State.Get_Number_Presets);
   package Row_Package is new Field_Package (Configuration.Row_Type,
      Configuration.Camera.State.State_Type,
      Configuration.Camera.State.Get_Number_Rows);

   ----------------------------------------------------------------
   procedure Create (
      Configured_Card            : in out Configured_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.View.View_Base_Type'Class;
      Camera_ID                  : in     Camera.Camera_ID_Type :=
                                             Camera.Null_Camera_ID) is
   ----------------------------------------------------------------

--    State       : Camera.States.State_Type'class renames
--                   Camera.States.Get_Read_Only_Global_State.all;
      Configuration_State
                        : Configuration.Camera.State.State_Type renames
                           Camera.States.Get_Read_Only_Configuration_State (
                              Camera_ID).all;
      Number_Configurations
                  : constant Configuration.Configuration_ID_Type :=
                     Configuration_State.Get_Number_Configurations;
   begin
      Log_In (Debug);

      Configured_Package.Widget_Type (Configured_Card).Create (
         Main_Window    => Main_Window,
         Parent         => Cards,
         Parent_Form    => Null,
         Number_Columns => Preset_Column_Index_Type'last,
         Number_Rows    => Configuration.Row_Type (Number_Configurations),
         Name           => Widget_Name);

      Configured_Card.Class_Name (Configuration.Camera.Configured_Card_Style);
      Log_Out (Debug, Quote ("Configured_Card property style",
         Configured_Card.Property ("style")) &
         Quote ("Configured_Card style", Configured_Card.Style ("style")));

   exception
      when Fault: others =>
         Log_Exception (True, Fault, "creating a Configured card");
         raise;

   end Create;

   -------------------------------------------------------------------
   function Create_Labels (
      Number_Columns             : in     Preset_Column_Index_Type
   ) return Column_Labels_Access is
   pragma Unreferenced (Number_Columns);
   -------------------------------------------------------------------

   begin
      Log_Here (Debug);
      return Column_Labels'access;
   end Create_Labels;

   ----------------------------------------------------------------
   overriding
   function Get_Accept_Button (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class is
   ----------------------------------------------------------------

   begin
      return Configured_Package.Widget_Type (
         Configured_Card).Get_Accept_Button;
   end Get_Accept_Button;

   ----------------------------------------------------------------
   overriding
   function Get_Cancel_Button (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class is
   ----------------------------------------------------------------

   begin
      return Configured_Package.Widget_Type (
         Configured_Card).Get_Cancel_Button;
   end Get_Cancel_Button;

   ----------------------------------------------------------------
   overriding
   function Get_Form (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Botton_Class is
   ----------------------------------------------------------------

   begin
      return Configured_Package.Widget_Type (
         Configured_Card).Get_Form;
   end Get_Form;

-- ---------------------------------------------------------------
-- procedure Update_Preset_Number (
--    State                      : in     State_Type;
--    Value_Field                : in out Camera.Preset_Range_Type;
--    Form_Field                 : in out Gnoga.Gui.Element.Form.Number_Type;
--    Value                      : in     Camera.Preset_Range_Type;
--    From                       : in     String := Ada_Lib.Trace.Here) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "value " & Value'img &
--       " max " & Get_Number_Presets (State)'img &
--       " was" & Value_Field'img & Quote (" field", Form_Field.Value) &
--       " from " & From);
--    Value_Field := Value;
--    if Value > Get_Number_Presets (State) then
--       Form_Field.Value ("");
--    else
--       Form_Field.Value (Integer (Value));
--    end if;
--    Log_Out (Debug, "new" & Value_Field'img &
--       Quote (" field", Form_Field.Value));
-- end Update_Preset_Number;

-- ----------------------------------------------------------------
-- overriding
-- procedure Verify_Widget (
--    Widget                     : in     Configured_Card_Type;
--    Verify_Parameter           : in     Widgets.Generic_Table.
--                                           Verify_Parameter_Class_Access) is
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Verify_Parameter.Verify_Widget (Widget);
--    Log_Out (Debug);
-- end Verify_Widget;

   package body Preset_Package is

      procedure Column_Package_Update (
         Cell                 : in out Cell_Type'class;
         Coordinate           : in     Configuration.Column_Type);

--    procedure Refresh_Row (
--       Row                        : in     Gnoga.Gui.Base.Pointer_To_Base_Class);

      procedure Row_Package_Update (
         Cell                 : in out Cell_Type'class;
         Coordinate           : in     Configuration.Row_Type
      )  with Pre => Camera.Main.Has_Main_Window_Connection;

      procedure Select_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record);

      procedure Update_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class
      ) with Pre  => GNOGA_Ada_Lib.Has_Parent (Object);

      procedure Update_Preset_Cell (
         Configured_Card            : in out Configured_Card_Type;
         Configuration_ID           : in     Configuration.Configuration_ID_Type;
         Preset_Cell                : in out Preset_Package.Preset_Cell_Type
      ) with Pre => Camera.Main.Has_Main_Window_Connection;

      ----------------------------------------------------------------
      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Class_Access;
         Column_Index            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Local_Column            : constant Preset_Column_Access :=
                                    new Preset_Column_Type;
      begin
         Log_In (Debug, "column " & Column_Index'img &
            " row " & Table_Row'img);
         Column := Generic_Cell_Package.GNOGA_Column_Class_Access (Local_Column);

         case Column_Index is
            when Row_Header =>
               Local_Column.Cell := new Row_Header_Cell_Type;

            when Label_Field =>
               Local_Column.Cell := new Label_Cell_Type;

            when Preset_Field =>
               Local_Column.Cell := new Preset_Cell_Type;

            when Column_Field =>
               Local_Column.Cell := new Column_Cell_Type;

            when Row_Field =>
               Local_Column.Cell := new Row_Cell_Type;

            when Image_Field =>
               Local_Column.Cell := new Image_Cell_Type;

            when Control_Grid_Field =>
               Local_Column.Cell := new Control_Grid_Cell_Type;

         end case;
         Log_Out (Debug);
      end Allocate_Column;

      ----------------------------------------------------------------
      procedure Button_Click_Handler (
         Button                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      ----------------------------------------------------------------

         Connection_Data         : Camera.Main.Window_Connection_Type'class renames
                                    Camera.Main.Window_Connection_Type'class (
                                       Button.Connection_Data.all);
         Value                   : constant String :=
                                    Gnoga.GUI.Element.Common.Button_Type (
                                       Button).Text;
         Configuration_ID        : constant Configuration.Configuration_ID_Type :=
                                       Configuration.Configuration_ID_Type'value (Value);
         Preset_ID               : constant Camera.Preset_ID_Type :=
                                    Configuration.Camera.Setup.Global_Camera_Setup.
                                       Configuration_Preset (Configuration_ID);
      begin
         Log_In (Debug, Quote ("button text", Value) &
            " Configuration_ID" & Configuration_ID'img &
            " button tag " & Tag_Name (Button'tag));

         Connection_Data.Process_Command (
            Camera.Memory_Set,
            Options     => (
               1 => (
                  Data           => Camera.Data_Type (Preset_Id.Get_ID),
                  Mode           => Camera.Fixed,
                  Start          => 6
               )
            ));
         Log_Out (Debug);
      end Button_Click_Handler;

      -------------------------------------------------------------
      procedure Column_Package_Update (
         Cell                 : in out Cell_Type'class;
         Coordinate           : in     Configuration.Column_Type) is
      -------------------------------------------------------------

         Column_Cell    : Column_Cell_Type renames
                           Column_Cell_Type (Cell);
         State          : Configuration.Camera.State.State_Type renames
                           Camera.States.Get_Read_Only_Configuration_State.all;
begin
         Column_Package.Update (State, Column_Cell.Column_Number,
            Column_Cell.Column_Coordinate, Coordinate);
      end Column_Package_Update;
--         -------------------------------------------------------------
--         procedure Column_Package_Update (
----          Cell                 : in     Column_Cell_Type;
--            Coordinate           : in     Configuration.Column_Type) is
--         -------------------------------------------------------------
--
--            Column_Cell          : Column_Cell_Type renames
--                                    Column_Cell_Type (Cell.all);
----          State                : Camera.States.State_Type renames
----                                  Camera.States.
----                                     Get_Read_Only_Global_State.all;
--         begin
--            Column_Package.Update (State, Column_Cell.Column_Number,
--               Column_Cell.Column_Coordinate, Coordinate);
--         end Column_Package_Update;


      ----------------------------------------------------------------
--    overriding
      procedure Create (
         Element                 : in out Gnoga.Gui.Element.Form.Text_Type;
         Form                    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Value                   : in     String := "";
         Name                    : in     String := "";
         ID                      : in     String := "") is
      ----------------------------------------------------------------

      begin
         Log_In (Debug, Quote ("value", Value) & Quote (" name", Name) &
            Quote (" id", ID));
         Gnoga.Gui.Element.Form.Text_Type (Element).Create (
            Form     => Form,
            Id       => Id,
            Name     => Name,
            Size     => 20,
            Value    => Value);
         Log_Out (Debug);
      end Create;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell           : in out Cell_Type;
         Form           : in     Gnoga.Gui.Element.Form.
                                    Pointer_To_Botton_Class;
         Row            : in out Gnoga.Gui.Element.Table.
                                    Table_Row_Type'class;
         Column         : in out Generic_Cell_Package.GNOGA_Column_Type'class;
         Table_Column   : in     Preset_Column_Index_Type;
         Table_Row      : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Configuration_ID        : Preset_Row_Index_Type renames
                                    Preset_Row_Index_Type (Table_Row);
         Name                    : constant String := Row.ID;
         Cell_ID                 : constant String := Name & "_Cell_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
      begin
         Log_In (Debug, "row " & Table_Row'img &
            " Column " & Table_Column'img &
            Quote (" cell ID", Cell_ID) &
            Quote (" column ID", Column.ID) &
            " Configuration_ID" & Configuration_ID'img);

         Cell.Configuration_ID := Configuration_ID;

         declare
            Preset_ID   : constant Camera.Preset_ID_Type :=
                           Configuration.Camera.Setup.Global_Camera_Setup.
                              Get_Preset_ID (Configuration_ID);
            Has_Preset  : constant Boolean :=
                           Configuration.Camera.Setup.Global_Camera_Setup.
                              Has_Preset (Preset_ID);
         begin
            Log_Here (Debug, " preset " & Preset_ID.Image &
               " has preset " & Has_Preset'img);

            if    Configuration_ID =
                     Configuration.Configuration_ID_Type'first or else
                  Table_Column /= Control_Grid_Field then
               Log_Here (Debug, Quote ("cell id", Cell_ID));
               Cell.Create (Column, ID => Cell_ID);
            else -- its the cell for Control Widget
               Log_Out (Debug, "Configuration_ID" & Configuration_ID'img &
                  " Table_Column " & Table_Column'img);
               return;
            end if;

         end;
         Cell.Dump (Debug, "");
         Log_Out (Debug);

      exception

         when Fault: others =>
            Log_Exception (Debug, Fault);
            raise;

      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Column_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Configuration_ID        : Preset_Row_Index_Type renames
                                    Preset_Row_Index_Type (Table_Row);
         Name                    : constant String := Row.ID;
         Field_ID               : constant String := Name & "_Field_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration_ID);
         Has_Preset  : constant Boolean :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Has_Preset (Preset_ID);
         Preset      : constant
                        Configuration.Camera.Setup.Preset_Type'class :=
                           (if Has_Preset then
                              Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset (
                                       Preset_ID)
                           else
                               Configuration.Camera.Setup.Null_Preset);
      begin
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         Cell.Column_Coordinate.Create (
            ID             => Field_ID,
            Form           => Form.all,
            Value          => (if Has_Preset then
                                 Trim (Preset.Column'img)
                              else
                                 ""));
         Cell.Column_Number := Preset.Column;
         Cell.Column_Coordinate.Class_Name (Configuration.Camera.Coordinate_Style);
         Cell.Column_Coordinate.Parent (Cell'unchecked_access);
         Cell.Column_Coordinate.On_Focus_Out_Handler (
            Update_Handler'access);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Control_Grid_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Connection_Data         : Camera.Main.Window_Connection_Type'class renames
                                    Camera.Main.Window_Connection_Type'class (
                                       Form.Connection_Data.all);
         Configuration_ID        : Preset_Row_Index_Type renames
                                    Preset_Row_Index_Type (Table_Row);
         Name                    : constant String := Row.ID;
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration_ID);

      begin
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         Cell.Table_Row := Table_Row;
         if Configuration_ID =
               Configuration.Configuration_ID_Type'first then
            declare
               ID          : constant String := Name & "_Control_Table";

            begin
               Log_Here (Debug, "preset id" & Preset_ID.Image &
                  Quote (" id", ID) &
                  Quote (" row id", Row.ID) &
                  Quote (" column id", Column.ID) &
                  " number presets" &
                     Video.Lib.Get_Last_Preset_ID'img);
               Cell.Control_Table.Create (
                  Connection_Data.Get_Main_Window.all,
                  Parent   => Column,
                  ID       => ID);
            end;
         end if;
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Image_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Configuration_ID        : Preset_Row_Index_Type renames
                                    Preset_Row_Index_Type (Table_Row);
         Name                    : constant String := Row.ID;
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration_ID);
         Has_Preset  : constant Boolean :=
                        Configuration.Camera.Setup.Global_Camera_Setup.Has_Preset (Preset_ID);
         State       : Configuration.Camera.State.State_Type renames
                        Camera.States.Get_Read_Only_Configuration_State.all;

      begin
         Log_In (Debug, "Preset_ID " & Preset_ID.Image &
            " has preset " & Has_Preset'img);
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         if Has_Preset then
            declare
               Configuration_Row_Index
                     : constant Configuration.Row_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.Preset_Row (Preset_ID);
               Configuration_Column_Index
                     : constant Configuration.Column_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.Preset_Column (Preset_ID);
               Has_Image
                     : constant Boolean := State.Has_Image (
                        Configuration_Row_Index,
                        Configuration_Column_Index);
               Image : Gnoga.Gui.Element.Common.IMG_Type
                        renames Cell.Image_Div.Image;
               Image_Id
                     : constant String := Name & "_Image_" &
                           Table_Column'img;
            begin
               Log_Here (Debug, "row" & Configuration_Row_Index'img &
                  " column" & Configuration_Column_Index'img &
                  " has image " & Has_Image'img);
               if Has_Image then
                  declare
                     Image_Path
                           : constant String :=
                              Configuration.Camera.State.Image_Name (
                                 Row   => Configuration_Row_Index,
                                 Column=> Configuration_Column_Index);
                  begin
                     Log_Here (Debug,
                        "configuration row" & Configuration_Row_Index'img &
                        " column" & Configuration_Column_Index'img &
                        Quote (" image path", Image_Path));
                     Cell.Image_Div.Path.Construct (Image_Path);
                     Image.Create (Cell, Image_Path, "", Image_Id);
                  end;
               else
                  Log_Here (Debug, "path " & Configuration.Camera.Blank_Preset);
                  Cell.Image_Div.Path.Construct (Configuration.Camera.Blank_Preset);
                  Image.Create (
                     Cell, Configuration.Camera.Blank_Preset, Configuration.Camera.Blank_Preset, Image_Id);
               end if;
               Image.Class_Name (Configuration.Camera.Control_Image_Style);
            end;
         end if;
         Cell.Dump (Debug, Here);
         Log_Out (Debug);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Label_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Name              : constant String := Row.ID;
         Field_ID          : constant String := Name & "_Field_" &
                              Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                              Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID         : constant Camera.Preset_ID_Type :=
                              Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset_ID (
                                 Configuration.Configuration_ID_Type (Table_Row));
         Has_Preset        : constant Boolean :=
                              Configuration.Camera.Setup.Global_Camera_Setup.Has_Preset (Preset_ID);
      begin
         Log_In (Debug);
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         declare
            Value          : constant String := (if Has_Preset then
                                 Configuration.Camera.Setup.Global_Camera_Setup.Configuration_Label (
                                    Configuration.Configuration_ID_Type (
                                       Table_Row))
                              else
                                 "");
         begin
            Log_Here (Debug, Quote ("field id", Field_ID) &
               Quote (" value", Value));
            Cell.Label.Create (
               ID             => Field_ID,
               Form           => Form.all,
               Size           => 20,
               Value          => Value);
            Cell.Label.On_Focus_Out_Handler (Update_Handler'access);
         end;
--       Cell.Label.Parent (Cell);
         Log_Out (Debug);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Preset_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Name        : constant String := Row.ID;
         Cell_ID     : constant String := Name & "_Cell_" &
                        Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                        Ada_Lib.Strings.Trim (Table_Column'img);
         Field_ID    : constant String := Name & "_Field_" &
                        Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                        Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration.Configuration_ID_Type (Table_Row));
         Has_Preset  : constant Boolean :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Has_Preset (Preset_ID);
      begin
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         declare
            ID       : constant Camera.Preset_Range_Type := Preset_ID.Get_ID;
            Value    : constant String := (if Has_Preset then
                        Trim (
                           Configuration.Camera.Setup.Global_Camera_Setup.Configuration_Preset (
                              Configuration.Configuration_ID_Type (Table_Row))'img)
                     else
                        "");
         begin
            Log_Here (Debug,  "ID" & ID'img &
               Quote (" cell id", Cell_ID) &
               Quote (" field id", Field_ID) &
               Quote (" value", Value));
            Cell.Preset_ID_Field.Create (
               ID             => Field_ID,
               Form           => Form.all,
               Value          => ID'img);

            Cell.Preset_Id := Preset_ID;
            Cell.Preset_ID_Field.Class_Name (Configuration.Camera.Preset_Style);
            Cell.Preset_ID_Field.On_Focus_Out_Handler (
               Update_Handler'access);
            Cell.Preset_ID_Field.On_Mouse_Click_Handler (
               Select_Handler'access);
            Cell.Preset_ID_Field.Parent (Cell);
         end;
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Row_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Configuration_ID        : Configuration.Row_Type renames Table_Row;
         Name                    : constant String := Row.ID;
         Field_ID               : constant String := Name & "_Field_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration.Configuration_ID_Type (
                              Configuration_ID));
         Has_Preset  : constant Boolean :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Has_Preset (Preset_ID);
         Preset      : constant
                        Configuration.Camera.Setup.Preset_Type'class :=
                           (if Has_Preset then
                              Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset (
                                       Preset_ID)
                           else
                               Configuration.Camera.Setup.Null_Preset);
      begin
         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);
         Cell.Row_Coordinate.Create (
            ID             => Field_ID,
            Form           => Form.all,
            Value          => (if Has_Preset then
                                 Trim (Preset.Row'img)
                              else
                                 ""));
         Cell.Row_Number := Preset.Row;
         Cell.Row_Coordinate.Class_Name (Configuration.Camera.Coordinate_Style);
         Cell.Row_Coordinate.Parent (Cell);
         Cell.Row_Coordinate.On_Focus_Out_Handler (Update_Handler'access);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Row_Header_Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                          Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Row_Type) is
      ----------------------------------------------------------------

         Configuration_ID        : Configuration.Row_Type renames Table_Row;
         Name                    : constant String := Row.ID;
         Field_ID               : constant String := Name & "_Field_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID   : constant Camera.Preset_ID_Type :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Get_Preset_ID (Configuration.Configuration_ID_Type (
                              Table_Row));
         Has_Preset  : constant Boolean :=
                        Configuration.Camera.Setup.Global_Camera_Setup.
                           Has_Preset (Preset_ID);
      begin
         Log_In (Debug, "Table_Column " & Table_Column'img &
            " Table_Row " & Table_Row'img);

         Cell_Type (Cell).Create_Cell (Form, Row, Column, Table_Column,
            Table_Row);

         Cell.Button.Create (
            Content        => (if Has_Preset then
                                 Configuration_ID'img
                              else
                                 ""),
            ID             => Field_ID,
            Parent         => Cell);

         Cell.Button.On_Click_Handler (
            Button_Click_Handler'Unrestricted_Access);
         Log_Out (Debug);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      function Create_Column (
         Widget                  : in out Widget_Type;
         Row                     : in     Configuration.Row_Type;
         Column                  : in     Preset_Column_Index_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Column;

      -------------------------------------------------------------------
      overriding
      procedure Create_Column (
         Column                     : in out Preset_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Configuration.Row_Type;
         Row_Index                  : in     Configuration.Row_Type;
         Column_Index               : in     Preset_Column_Index_Type;
         ID                         : in     String) is
      -------------------------------------------------------------------

      begin
         Log_In (Debug, "column " & Column'img & " row " & Row'img &
            " number rows" & Number_Rows'img & " row index" & Row_Index'img &
            " column index " & Column_Index'img & " id " & ID'img);
         Column.Create (Row, "row" & Row_Index'img);
         Log_Out (Debug);
      end Create_Column;

      ----------------------------------------------------------------
      overriding
      function Create_Row (
         Widget                  : in out Widget_Type;
         Row                     : in     Configuration.Row_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Row;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Put_Line ("dump (" & Here & ") " & (
               if Caller'length > 0 then
                  " cell called from " & Caller
               else
                  "") &
            " from " & From & " address: " & Image (Cell'address));
            Put_Line ((if Cell.Configuration_ID =
                  Configuration.No_Configuration then
               "configuration id not set"
            else
               "  Configuration_ID:" & Cell.Configuration_ID'img));
         end if;

      exception
         when Fault: others =>
            Log_Exception (True, Fault);
            raise;

      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Column_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line ("  Column_Number:" & Cell.Column_Number'img);
               Put_Line (Quote ("  Column_Coordinate",
               Cell.Column_Coordinate.Value) &
               " dump " & Here);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Control_Grid_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Image_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line (Quote ("  Image path", Cell.Image_Div.Path) &
               " dump " & Here);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Label_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line (Quote ("label", Cell.Label.Text) &
               " dump " & Here);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Preset_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line ("  Preset_ID:" & Cell.Preset_ID.Image);
               Put_Line (Quote ("  Preset_ID_Field:",
               Cell.Preset_ID_Field.Value) &
               " dump " & Here);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Row_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line ("  Row_Number:" & Cell.Row_Number'img);
               Put_Line (Quote ("  Row_Coordinate:",
               Cell.Row_Coordinate.Value) &
               " dump " & Here);
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Row_Header_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Cell_Type (Cell).Dump (Enable, Caller, From);
               Put_Line (Quote ("  Button", Cell.Button.Text) &
               " dump " & Here);
         end if;
      end Dump;

--    ----------------------------------------------------------------
--    overriding
--    function Get_Cell (
--       Column                  : in out Preset_Column_Type;
--       From                    : in     String := Ada_Lib.Trace.Here
--    ) return Generic_Cell_Package.Cell_Class_Access is
--    ----------------------------------------------------------------
--
--    begin
--       return Generic_Cell_Package.Cell_Class_Access'(
--          Generic_Cell_Package.Cell_Class_Access (Column.Cell));
--    end Get_Cell;

      ----------------------------------------------------------------
      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Log_Out (Debug);
      end On_Submit;

--    ----------------------------------------------------------------
--    procedure Refresh_Row (
--       Row                        : in     Gnoga.Gui.Base.Pointer_To_Base_Class) is
--    ----------------------------------------------------------------
--
--    begin
--       Log_In (Debug);
--       Log_Out (Debug);
--    end Refresh_Row;

      -------------------------------------------------------------
      procedure Row_Package_Update (
         Cell                 : in out Cell_Type'class;
         Coordinate           : in     Configuration.Row_Type) is
      -------------------------------------------------------------

         Row_Cell       : Row_Cell_Type renames Row_Cell_Type (Cell);
         State          : Configuration.Camera.State.State_Type renames
                           Camera.States.Get_Read_Only_Configuration_State.all;
      begin
         Row_Package.Update (State, Row_Cell.Row_Number,
            Row_Cell.Row_Coordinate, Coordinate);
      end Row_Package_Update;

      ----------------------------------------------------------------
      procedure Select_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record) is
      ----------------------------------------------------------------

         Connection_Data   : Camera.Main.Window_Connection_Type'class renames
                              Camera.Main.Window_Connection_Type'class (
                                 Object.Connection_Data.all);
         Cell              : constant Preset_Package.Cell_Class_Access :=
                              Preset_Package.Cell_Class_Access (
                                 Object.Parent);
         Preset_Cell       : Preset_Cell_Type renames
                              Preset_Cell_Type (Cell.all);
      begin
         Log_In (Debug);
         if Mouse_Event.Left_Button then
            Connection_Data.Get_Camera.Set_Preset (Preset_Cell.Preset_ID);
         end if;
         Log_Out (Debug);
      end Select_Handler;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Column_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      ----------------------------------------------------------------

         Column_Number  : constant Configuration.Column_Type :=
                        Update_Parameter_Type (Update_Parameter).Column_Number;
         Value          : constant Integer := Update_Parameter_Type (
                           Update_Parameter).Column_Coordinate;
      begin
         Log_In (Debug, "Column number" & Column_Number'img &
            " value" & Value'img);
         Cell.Column_Coordinate.Value (Value);
         Cell.Column_Number := Column_Number;
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Control_Grid_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      pragma Unreferenced (Cell, Update_Parameter);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Not_Implemented;
--       Cell.Preset_ID := Preset_ID;
--       Cell.Preset_ID_Field.Value (Preset_ID.Image);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Image_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      pragma Unreferenced (Cell, Update_Parameter);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Not_Implemented;
--       Cell.Preset_ID := Preset_ID;
--       Cell.Preset_ID_Field.Value (Preset_ID.Image);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Label_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      pragma Unreferenced (Cell, Update_Parameter);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Not_Implemented;
--       Cell.Preset_ID := Preset_ID;
--       Cell.Preset_ID_Field.Value (Preset_ID.Image);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Preset_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Cell.Preset_ID := Update_Parameter_Type (Update_Parameter).Preset_ID;
         Cell.Preset_ID_Field.Value (Update_Parameter_Type (
            Update_Parameter).Preset_ID.Image);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Row_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      ----------------------------------------------------------------

         Row_Number  : constant Configuration.Row_Type :=
                        Update_Parameter_Type (Update_Parameter).Row_Number;
         Value       : constant Integer := Update_Parameter_Type (
                        Update_Parameter).Row_Coordinate;
      begin
         Log_In (Debug, "row number" & Row_Number'img &
            " value" & Value'img);
         Cell.Row_Coordinate.Value (Value);
         Cell.Row_Number := Row_Number;
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Row_Header_Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      pragma Unreferenced (Cell, Update_Parameter);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Not_Implemented;
--       Cell.Preset_ID := Preset_ID;
--       Cell.Preset_ID_Field.Value (Preset_ID.Image);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      procedure Update_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      ----------------------------------------------------------------

         Connection_Data         : Camera.Main.Window_Connection_Type'class renames
                                    Camera.Main.Window_Connection_Type'class (
                                       Object.Connection_Data.all);
--       Configured_Card         : Configured_Card_Type renames
--                                  Connection_Data.Get_Configured_Card.all;
         Cell                    : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Object.Parent);
--       Cell_Tag                : constant Ada.Tags.Tag := Cell.all'tag;
         Camera_Configuration    : constant Configuration.Camera.Setup.
                                    Configuration_Type'class :=
                                       Configuration.Camera.Setup.Global_Camera_Setup.
                                          Get_Configuration (Cell.Configuration_ID);
--       ID                      : constant String :=
--                                  Configured_Card.ID;
         Preset                  : constant Configuration.Camera.Setup.Preset_Type'class :=
                                    Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset (
                                       Camera_Configuration.Preset_ID);
--       State                   : Camera.States.State_Type
--                                  renames Standard.Camera.States.
--                                  Get_Read_Only_Global_State.all;

         -------------------------------------------------------------
         function Column_Cell_Coordinate
         return Integer is
         -------------------------------------------------------------

            Column_Cell    : Column_Cell_Type renames Column_Cell_Type (Cell.all);

         begin
            return Column_Cell.Column_Coordinate.Value;
         end Column_Cell_Coordinate;

         -------------------------------------------------------------
         function Column_Cell_Number
         return Configuration.Column_Type is
         -------------------------------------------------------------

            Column_Cell    : Column_Cell_Type renames Column_Cell_Type (Cell.all);

         begin
            return Column_Cell.Column_Number;
         end Column_Cell_Number;

--         -------------------------------------------------------------
--         procedure Row_Package_Update (
--            Coordinate           : in     Row_Type) is
--         -------------------------------------------------------------
--
--            Row_Cell             : Row_Cell_Type renames Row_Cell_Type (Cell.all);
----          State                : Camera.States.State_Type renames
----                                  Camera.States.
----                                     Get_Read_Only_Global_State.all;
--         begin
--            Row_Package.Update (State, Row_Cell.Row_Number,
--               Row_Cell.Row_Coordinate, Coordinate);
--         end Row_Package_Update;

--         -------------------------------------------------------------
--         procedure Update_Coordinate is
--         -------------------------------------------------------------
--
--         begin
--            Log_Here (Debug, "coordinate " & Coordinate_Name &
--               Quote (" coordinate value ", Cell_Coordinate) &
--               " cell number" & Cell_NUmber'img);
--            declare
--               Raw_Coordinate    : constant Integer :=
--                                    Integer'Value (Cell_Coordinate);
--            begin
--               Log_Here (Debug,
--                  " new " & Coordinate_Name & Raw_Coordinate'img &
--                  " old " & Coordinate_Name & Cell_Number'img &
--                  " column" & Preset_Other_Coordinate (Preset)'img);
--
--               if Row_Package.Valid (State, Raw_Coordinate) then
--                  declare
----                   Current_Preset_Cell : constant Cell_Class_Access := Cell_Class_Access (
----                                  Configured_Card.Get_Cell (Preset_Field,
----                                     Cell.Configuration_ID));
--                     Image_Cell  : Image_Cell_Type renames
--                                    Image_Cell_Type (Cell.all);
--                     New_Coordinate
--                                 : constant Coordinate_Type :=
--                                    Coordinate_Type (Raw_Coordinate);
--                     New_Preset_ID
--                                 : Camera.Preset_ID_Type :=
--                                    Generic_Preset_ID (New_Coordinate,
--                                       Preset_Other_Coordinate (Preset));
--                     New_Preset_Number
--                                 : Natural := 0;
--                     Preset_Cell : Preset_Cell_Type renames
--                                    Preset_Cell_Type (Cell.all);
--                  begin
--                     Log_Here (Debug,
--                        " new preset id" & New_Preset_ID'img &
--                        " other coordinate" & Preset_Other_Coordinate (Preset)'img);
--
--                     Package_Update (Cell.all, New_Coordinate);
--                     if not New_Preset_ID.Is_Set then
--                        Log_Here (Debug);
--                        Preset_Cell.Preset_ID := Video.Lib.Null_Preset_ID;
--                        Preset_Cell.Preset_ID_Field.Value ("");
--                        Image_Cell.Image_Div.Image.URL_Source (Blank_Preset);
--                        Image_Cell.Image_Div.Path.Construct (Blank_Preset);
--                     else
--                        Log_Here (Debug);
--                        declare
--                           Path  : constant String :=
--                                    Generic_Check_Image (
--                                       New_Coordinate    => New_Coordinate,
--                                       Other_Coordinate  =>
--                                          Preset_Other_Coordinate (Preset));
--                        begin
--                           Log_Here (Debug, Quote ("image", Path));
--                           Configuration.Camera.Setup.Global_Camera_Setup.Update_Configuration (
--                              Cell.Configuration_ID, New_Preset_ID);
--                           Preset_ID_Package.Update (
--                              State, New_Preset_Number,
--                              Preset_Cell.Preset_ID_Field, Natural (New_Preset_ID.Get_ID));
--
--                           New_Preset_ID.Set (Camera.Preset_Range_Type (New_Preset_Number));
--                           Image_Cell.Image_Div.Image.URL_Source (Path);
--                           Image_Cell.Image_Div.Path.Construct (Path);
--                        end;
--                     end if;
--                  end;
--               else
--                  Log_Here (Debug);
--                  -- put text of row number back in field
--                  Package_Update (Cell.all, Cell_Number);
--               end if;
--            end;
--         end Update_Coordinate;

         -------------------------------------------------------------
         function Row_Cell_Coordinate
         return Integer is
         -------------------------------------------------------------

            Row_Cell    : Row_Cell_Type renames Row_Cell_Type (Cell.all);

         begin
            return Row_Cell.Row_Coordinate.Value;
         end Row_Cell_Coordinate;

         -------------------------------------------------------------
         function Row_Cell_Number
         return Standard.Configuration.Row_Type is
         -------------------------------------------------------------

            Row_Cell    : Row_Cell_Type renames Row_Cell_Type (Cell.all);

         begin
            return Row_Cell.Row_Number;
         end Row_Cell_Number;

         -------------------------------------------------------------
         function Row_Check_Image (
            New_Coordinate    : in     Configuration.Row_Type;
            Other_Coordinate  : in     Configuration.Column_Type
         ) return String IS
         -------------------------------------------------------------

         begin
            return Configuration.Camera.State.Image_Name (
               Row      => New_Coordinate,
               Column   => Other_Coordinate);
         end Row_Check_Image;

         -------------------------------------------------------------
         function Row_Preset_Other_Coordinate (
            Preset               : in     Configuration.Camera.Setup.Preset_Type'class
         ) return Configuration.Column_Type is
         -------------------------------------------------------------

         begin
            return Preset.Column;
         end Row_Preset_Other_Coordinate;

         -------------------------------------------------------------
         function Row_Preset_ID (
            Coordinate           : in     Configuration.Row_Type;
            Other_Coordinate      : in     Configuration.Column_Type
         ) return Camera.Preset_ID_Type is
         -------------------------------------------------------------

         begin
            return Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset_ID (
               Row => Coordinate,
               Column   => Other_Coordinate);
         end Row_Preset_ID;

--       procedure Update_Row is new Update_Coordinate (
--          Coordinate_Name         => "row",
--          Coordinate_Type         => Row_Type,
--          Other_Coordinate_Type   => Column_Type,
--          Cell_Coordinate         => Row_Cell_Coordinate,
--          Cell_Number             => Row_Cell_Number,
--          Generic_Check_Image     => Row_Check_Image,
--          Preset_Other_Coordinate => Row_Preset_Other_Coordinate,
--          Generic_Preset_ID       => Row_Preset_ID,
--          Package_Update          => Row_Package_Update);

--       -------------------------------------------------------------
--       function Column_Cell_Coordinate
--       return Column_Type is
--       -------------------------------------------------------------
--
--       begin
--          return Cell.Column_Number;
--       end Column_Cell_Coordinate;

         -------------------------------------------------------------
         function Column_Check_Image (
            New_Coordinate    : in     Configuration.Column_Type;
            Other_Coordinate  : in     Configuration.Row_Type
         ) return String IS
         -------------------------------------------------------------

         begin
            return Configuration.Camera.State.Image_Name (
               Column   => New_Coordinate,
               Row      => Other_Coordinate);
         end Column_Check_Image;

         -------------------------------------------------------------
         function Column_Preset_Other_Coordinate (
            Preset               : in     Configuration.Camera.Setup.Preset_Type'class
         ) return Configuration.Row_Type is
         -------------------------------------------------------------

         begin
            return Preset.Row;
         end Column_Preset_Other_Coordinate;

         -------------------------------------------------------------
         function Column_Preset_ID (
            Coordinate           : in     Configuration.Column_Type;
            Other_Coordinate      : in     Configuration.Row_Type
         ) return Camera.Preset_ID_Type is
         -------------------------------------------------------------

         begin
            return Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset_ID (
               Column   => Coordinate,
               Row      => Other_Coordinate);
         end Column_Preset_ID;

--       procedure Update_Column is new Update_Coordinate (
--          Coordinate_Name         => "column",
--          Coordinate_Type         => Column_Type,
--          Other_Coordinate_Type   => Row_Type,
--          Cell_Coordinate         => Column_Cell_Coordinate,
--          Cell_Number             => Column_Cell_Number,
--          Generic_Check_Image     => Column_Check_Image,
--          Preset_Other_Coordinate => Column_Preset_Other_Coordinate,
--          Generic_Preset_ID       => Column_Preset_ID,
--          Package_Update          => Column_Package_Update);

      begin -- Update_Handler
         Log_In (Debug, -- Quote ("update field column " & Cell.Table_Column'img &
--          " id", ID) &
            Quote (" object id", Object.ID) &
            " tag " & Tag_Name (Cell.all'tag));
--          " configuration id" & Cell.Configuration_ID'img);

         if Debug then
            Configuration.Camera.Setup.Global_Camera_Setup.Dump;
         end if;

         Connection_Data.Reset_Update_Event;

         declare
            Update_Parameter  : constant Update_Parameter_Type := (
                                 Column_Coordinate => Column_Cell_Coordinate,
                                 Column_Number     => Preset.Column,
                                 Preset_ID         => Preset.Preset_ID,
                                 Row_Coordinate    => Row_Cell_Coordinate,
                                 Row_Number        => Preset.Row);
         begin
            Log_Here (Debug, "update parameters: " &
               " column corrdinate" & Update_Parameter.Column_Coordinate'img &
               " column number" & Update_Parameter.Column_Number'img &
               " preset id" & Update_Parameter.Preset_ID'img &
               " row corrdinate" & Update_Parameter.Row_Coordinate'img &
               " row number" & Update_Parameter.Row_Number'img);
            Cell.Update_Cell (Update_Parameter);
         end;
--       case Cell.Table_Column is
--         if Cell_Tag = Preset_Package.Column_Cell_Type'tag then
--
----          when Column_Field =>
--               Update_Column;
--
----          when Label_Field =>
--         elsif Cell_Tag = Preset_Package.Label_Cell_Type'tag then
--               declare
--                  Label_Cell     : Label_Cell_Type renames
--                                    Label_Cell_Type (Cell.all);
--                  New_Label      : constant String := Label_Cell.Label.Value;
--
--               begin
--                  Log_Here (Debug, Quote ("label", New_Label));
--                  Configuration.Camera.Setup.Global_Camera_Setup.Update_Configuration (
--                     Cell.Configuration_ID, New_Label);
--               end;
--
----          when Preset_Field =>
--         elsif Cell_Tag = Preset_Package.Preset_Cell_Type'tag then
--            declare
--               Preset_Cell    : Preset_Cell_Type renames
--                                 Preset_Cell_Type (Cell.all);
--            begin
--               Update_Preset_Cell (Configured_Card, Cell.Configuration_ID,
--                  Preset_Cell);
--            end;
--
----          when Row_Field =>
--         elsif Cell_Tag = Preset_Package.Row_Cell_Type'tag then
--               Update_Row;
--
----          when others =>
--         else
--               Log_Exception (Debug);
--               raise Failed with "handler not implemmented for " &
--                  Ada.Tags.Expanded_Name (Cell_Tag) & " at " & Here;
--
--         end if;

         Connection_Data.Trigger_Update_Event;
         if Debug then
            Configuration.Camera.Setup.Global_Camera_Setup.Dump;
         end if;
         Log_Out (Debug);

      exception

         when Fault: others =>
            Trace_Exception (Debug, Fault);
            raise;

      end Update_Handler;

      ----------------------------------------------------------------
      procedure Update_Preset_Cell (
         Configured_Card            : in out Configured_Card_Type;
         Configuration_ID           : in     Configuration.Configuration_ID_Type;
         Preset_Cell                : in out Preset_Package.Preset_Cell_Type) is
      ----------------------------------------------------------------

         Raw_Value      : constant String :=
                           Preset_Cell.Preset_ID_Field.Value;
         New_Preset_ID  : constant Camera.Preset_ID_Type :=
                           (if Raw_Value'length = 0 then
                              Video.Lib.Null_Preset_ID
                           else
                              Video.Lib.Constructor (
                                 Camera.Preset_Range_Type'value (
                                    Raw_Value)));
         State          : Configuration.Camera.State.State_Type renames
                           Camera.States.Get_Read_Only_Configuration_State.all;
      begin
         Log_In (Debug, "Configuration_ID" & Configuration_ID'img &
            Quote (" raw preset value", Raw_Value) &
            " new preset id" & New_Preset_ID'img);
         if not New_Preset_ID.Is_Set  then -- put back the previous preset
            Log_Here (Debug);
            Preset_Cell.Preset_ID_Field.Value (Preset_Cell.Preset_ID.Image);
         else
            Log_Here (Debug);
            declare
--             Column_Cell       :  Preset_Package.Column_Cell_Type
--                                     renames Preset_Package.Column_Cell_Type (
--                                        Configured_Card.Get_Cell (Column_Field,
--                                           Configuration_ID).all);
               Image_Cell        : Preset_Package.Image_Cell_Type
                                    renames Preset_Package.Image_Cell_Type (
                                          Configured_Card.Get_Cell (Image_Field,
                                             Configuration.Row_Type (Configuration_ID)).all);
               Preset            : constant
                                    Configuration.Camera.Setup.Preset_Type'class :=
                                       (if not New_Preset_ID.Is_Set then
                                             Configuration.Camera.Setup.Null_Preset
                                          else
                                             Configuration.Camera.Setup.Global_Camera_Setup.Get_Preset (
                                                New_Preset_ID));
               Preset_Number     : Natural := 0;
               Path              : constant String := Configuration.Camera.State.Image_Name (
                                    Row      => Preset.Row,
                                    Column   => Preset.Column);
--             Row_Cell          :  Preset_Package.Row_Cell_Type
--                                     renames Preset_Package.Row_Cell_Type (
--                                        Configured_Card.Get_Cell (Row_Field,
--                                           Configuration_ID).all);
            begin
               Log_Here (Debug, Quote ("image", Path));
               Configuration.Camera.Setup.Global_Camera_Setup.Update_Configuration (
                  Configuration_ID, Video.Lib.Constructor (
                     Camera.Preset_Range_Type (Preset_Number)));
               Preset_ID_Package.Update (State, Preset_Number,
                  Preset_Cell.Preset_ID_Field, Natural (New_Preset_ID.Get_ID));

               Column_Package_Update (Preset_Cell, Preset.Column);
               Image_Cell.Image_Div.Image.URL_Source (Path);
               Image_Cell.Image_Div.Path.Construct (Path);
               Row_Package_Update (Preset_Cell, Preset.Row);
            end;
         end if;
      end Update_Preset_Cell;

   end Preset_Package;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Widgets.Configured;

