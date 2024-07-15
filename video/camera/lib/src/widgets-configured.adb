with Ada.Text_IO; use  Ada.Text_IO;
--with Ada_Lib.GNOGA;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Base;
with Camera.Lib.Base;
with Configuration.Camera.Setup;
   use Configuration.Camera;
   use Configuration.Camera.Setup;
with Configuration.Camera.State; use Configuration.Camera.State;
--with Main;
--with Camera.Lib.Base;

package body Widgets.Configured is

-- subtype Configured_Cell_Class_Access
--                               is Generic_Cell_Package.Cell_Class_Access;

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

   package Column_Package is new Field_Package (Column_Type,
      State_Type,
      Get_Number_Columns);
   package Preset_ID_Package is new Field_Package (Preset_ID_Type,
      State_Type,
      Get_Number_Presets);
   package Row_Package is new Field_Package (Row_Type,
      State_Type,
      Get_Number_Rows);

   ----------------------------------------------------------------
   procedure Create (
      Configured_Card            : in out Configured_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.View.View_Base_Type'Class) is
   ----------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      Number_Configurations      : constant
                                    Configuration_ID_Type :=
                                       State.Number_Configurations;
   begin
      Log_In (Debug);
      Configured_Card.Create (
         Main_Window    => Main_Window,
         Parent         => Cards,
         Parent_Form    => Null,
         Number_Columns => Preset_Column_Index_Type'last,
         Number_Rows    => Number_Configurations,
         Name           => Widget_Name);
      Configured_Card.Class_Name (Configured_Card_Style);
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


   ----------------------------------------------------------------
   overriding
   procedure Verify_Widget (
      Widget                     : in     Configured_Card_Type;
      Verify_Parameter           : in     Configured_Package.
                                             Verify_Parameter_Class_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Verify_Parameter.Verify_Widget (Widget);
      Log_Out (Debug);
   end Verify_Widget;

   package body Preset_Package is

      procedure Column_Package_Update (
         Cell                 : in     Cell_Class_Access;
         Coordinate           : in     Column_Type
      ) with Pre =>Ada_Lib.GNOGA.Has_Connection_Data;

--    procedure Refresh_Row (
--       Row                        : in     Gnoga.Gui.Base.Pointer_To_Base_Class);

      procedure Row_Package_Update (
         Cell                 : in     Cell_Class_Access;
         Coordinate           : in     Row_Type
      )  with Pre =>Ada_Lib.GNOGA.Has_Connection_Data;

      procedure Select_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record);

      procedure Update_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

      procedure Update_Preset_Cell (
         Configured_Card            : in out Configured_Card_Type;
         Configuration_ID           : in     Configuration_ID_Type;
         Preset_Cell                : in out Preset_Package.Cell_Type'class
      ) with Pre => Ada_Lib.GNOGA.Has_Connection_Data;

      ----------------------------------------------------------------
      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration_ID_Type) is
      ----------------------------------------------------------------

         Local_Column            : constant Preset_Column_Access :=
                                    new Preset_Column_Type;
      begin
         Log_Here (Debug, "column " & Column_Index'img &
            " row " & Table_Row'img);
         Column := Generic_Cell_Package.Generic_Column_Class_Access (Local_Column);
         Local_Column.Cell := new Cell_Type (Column_Index);
      end Allocate_Column;

      ----------------------------------------------------------------
      procedure Button_Click_Handler (
         Button                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      ----------------------------------------------------------------

         Connection_Data         : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
         Value                   : constant String :=
                                    Gnoga.GUI.Element.Common.Button_Type (
                                       Button).Text;
         Configuration_ID        : constant Configuration_ID_Type
                                    :=
                                       Configuration_ID_Type'value (Value);
         Preset_ID               : constant Preset_ID_Type :=
                                    Global_Camera_Setup.
                                       Configuration_Preset (Configuration_ID);
      begin
         Log_In (Debug, Quote ("button text", Value) &
            " Configuration_ID" & Configuration_ID'img &
            " button tag " & Tag_Name (Button'tag));

         Connection_Data.Camera.Process_Command (
            Camera.Lib.Base.Memory_Set,
            Options     => (
               1 => (
                  Data           => Camera.Data_Type (Preset_ID),
                  Start          => 6,
                  Variable_Width => False
               )
            ));
         Log_Out (Debug);
      end Button_Click_Handler;

      -------------------------------------------------------------
      procedure Column_Package_Update (
         Cell                 : in     Cell_Class_Access;
         Coordinate           : in     Column_Type) is
      -------------------------------------------------------------

         Connection_Data      : Base.Connection_Data_Type renames
                                 Base.Connection_Data_Type (
                                    Ada_Lib.GNOGA.Get_Connection_Data.all);
         State                : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      begin
         Column_Package.Update (State, Cell.Column_Number,
            Cell.Column_Coordinate, Coordinate);
      end Column_Package_Update;

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
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                             Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Preset_Row_Index_Type) is
      ----------------------------------------------------------------

         Connection_Data         : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Form.Connection_Data.all);
         Configuration_ID        : Preset_Row_Index_Type renames Table_Row;
         Name                    : constant String := Row.ID;
         Cell_ID                 : constant String := Name & "_Cell_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
         Field_ID               : constant String := Name & "_Field_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                    Ada_Lib.Strings.Trim (Table_Column'img);
         Preset_ID               : constant Preset_ID_Type :=
                                    Global_Camera_Setup.
                                       Get_Preset_ID (Configuration_ID);
         Has_Preset              : constant Boolean :=
                                    Global_Camera_Setup.
                                       Has_Preset (Preset_Id);
         Preset                  : constant
                                    Preset_Type'class :=
                                       (if
                                          Global_Camera_Setup.Has_Preset (
                                                Preset_ID) then

                                                Global_Camera_Setup.Get_Preset (
                                                   Preset_ID)
                                       else
                                           Null_Preset);
         State                   : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      begin
         Log_In (Debug, "row " & Table_Row'img &
            " Column " & Table_Column'img &
            Quote (" cell ID", Cell_ID) &
            Quote (" column ID", Column.ID) &
            " preset id" & Preset_ID'img &
            " has preset " & Has_Preset'img);

         if    Configuration_ID =
                  Configuration_ID_Type'first or else
               Table_Column /= Control_Field then
            Log_Here (Debug);
            Cell.Create (Column, ID => Cell_ID);
         else -- its the cell for Control Widget
            Log_Out (Debug, "Configuration_ID" & Configuration_ID'img &
               " Table_Column " & Table_Column'img);
            return;
         end if;

         Cell.Configuration_ID := Table_Row;

         case Table_Column is

            when Column_Field =>
               Cell.Column_Coordinate.Create (
                  ID             => Field_ID,
                  Form           => Form.all,
                  Value          => (if Has_Preset then
                                       Trim (Preset.Column'img)
                                    else
                                       ""));
               Cell.Column_Number := Preset.Column;
               Cell.Column_Coordinate.Class_Name (Coordinate_Style);
               Cell.Column_Coordinate.Parent (Cell'unchecked_access);
               Cell.Column_Coordinate.On_Focus_Out_Handler (
                  Update_Handler'access);

            when Control_Field =>
               if Configuration_ID =
                     Configuration_ID_Type'first then
                  declare
                     ID          : constant String := Name & "_Control_Table";

                  begin
                     Log_Here (Debug, "preset id" & Preset_ID'img &
                        Quote (" id", ID) &
                        Quote (" row id", Row.ID) &
                        Quote (" column id", Column.ID) &
                        " number presets" &
                           State.Last_Preset'img);
                     Cell.Control_Table.Create (
                        Connection_Data.Get_Main_Window.all,
                        Parent   => Column,
                        ID       => ID);
                  end;
               end if;

            when Image_Field =>
               Log_Here (Debug, "Preset_ID" & Preset_ID'img);
               if Global_Camera_Setup.Has_Preset (Preset_ID) then
                  declare
                     Configuration_Row_Index
                           : constant Row_Type :=
                              Global_Camera_Setup.Preset_Row (Preset_ID);
                     Configuration_Column_Index
                           : constant Column_Type :=
                              Global_Camera_Setup.Preset_Column (Preset_ID);
                     Image : Gnoga.Gui.Element.Common.IMG_Type
                              renames Cell.Image_Div.Image;
                     Image_Id
                           : constant String := Name & "_Image_" &
                                 Table_Column'img;
                  begin
                     Log_Here (Debug, "row" & Configuration_Row_Index'img &
                        " column" & Configuration_Column_Index'img);
                     if State.Has_Image (
                           Configuration_Row_Index,
                           Configuration_Column_Index) then
                        declare
                           Image_Path
                                 : constant String :=
                                    Image_Name (
                                       Row   => Configuration_Row_Index,
                                       Column=> Configuration_Column_Index);
                        begin
                           Log_Here (Debug,
                              "configuration row" & Configuration_Row_Index'img &
                              " column" & Configuration_Column_Index'img &
                              Quote (" image path", Image_Path));
                           Cell.Image_Div.Path.Construct (Image_Path);
                           Image.Create (
                              Cell, Image_Path, "", Image_Id);
                        end;
                     else
                        Image.Create (
                           Cell, Blank_Preset, "", Image_Id);
                     end if;
                     Image.Class_Name (Control_Image_Style);
                  end;
               end if;

            when Label_Field =>
               declare
                  Value          : constant String := (if Has_Preset then
                                       Global_Camera_Setup.Configuration_Label (
                                          Configuration_ID)
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

                  Cell.Label.Parent (Cell);
                  Cell.Label.On_Focus_Out_Handler (Update_Handler'access);
               end;

            when Preset_Field =>
               declare
                  Value          : constant String := (if Has_Preset then
                                    Trim (
                                       Global_Camera_Setup.Configuration_Preset (
                                          Configuration_ID)'img)
                                 else
                                    "");
               begin
                  Log_Here (Debug, Quote ("field id", Field_ID) &
                     Quote (" value", Value));
                  Cell.Preset_ID_Field.Create (
                     ID             => Field_ID,
                     Form           => Form.all,
                     Value          => Value);

                  Cell.Preset_Id := Preset_ID;
                  Cell.Preset_ID_Field.Class_Name (Preset_Style);
                  Cell.Preset_ID_Field.On_Focus_Out_Handler (
                     Update_Handler'access);
                  Cell.Preset_ID_Field.On_Mouse_Click_Handler (
                     Select_Handler'access);
                  Cell.Preset_ID_Field.Parent (Cell);
               end;

            when Row_Field =>
               Cell.Row_Coordinate.Create (
                  ID             => Field_ID,
                  Form           => Form.all,
                  Value          => (if Has_Preset then
                                       Trim (Preset.Row'img)
                                    else
                                       ""));
               Cell.Row_Number := Preset.Row;
               Cell.Row_Coordinate.Class_Name (Coordinate_Style);
               Cell.Row_Coordinate.Parent (Cell);
               Cell.Row_Coordinate.On_Focus_Out_Handler (Update_Handler'access);

            when Row_Header =>
               Cell.Button.Create (
                  Content        => (if Has_Preset then
                                       Configuration_ID'img
                                    else
                                       ""),
                  ID             => Field_ID,
                  Parent         => Cell);

               Cell.Button.On_Click_Handler (
                  Button_Click_Handler'Unrestricted_Access);

         end case;
         Cell.Dump (Debug);
         Log_Out (Debug);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Column (
         Column                     : in out Preset_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Preset_Row_Index_Type;
         Row_Index                  : in     Preset_Row_Index_Type;
         Column_Index               : in     Preset_Column_Index_Type;
         ID                         : in     String) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug, "number rows " & Number_Rows'img &
            " row " & Row_Index'img &
            " column " & Column_Index'img & " id " & ID);

         Gnoga.Gui.Element.Table.Table_Column_Type (Column).Create (
            Column_Span    => 1,
            Content        => "",
            ID             => ID,
            Row            => Row,
            Row_Span       => (if   Row_Index =
                                       Preset_Row_Index_Type'first and then
                                    Column_Index =
                                       Preset_Column_Index_Type'last then
                                 Positive (Number_Rows)
                              else
                                 1));

         Log_Out (Debug);
      end Create_Column;

      ----------------------------------------------------------------
      overriding
      function Create_Column (
         Widget                  : in out Widget_Type;
         Row                     : in     Preset_Row_Index_Type;
         Column                  : in     Preset_Column_Index_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Column;

      ----------------------------------------------------------------
      overriding
      function Create_Row (
         Widget                  : in out Widget_Type;
         Row                     : in     Preset_Row_Index_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Row;

      ----------------------------------------------------------------
      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         if Enable then
            Put_Line ("dump cell called from " & From &
               " address: " & Image (Cell'address));
            Put_Line ("  Column:" & Cell.Column'img);
            Put_Line ("  Configuration_ID:" & Cell.Configuration_ID'img);
            case Cell.Column is

               when Column_Field=>
                  Put_Line ("  Column_Number:" & Cell.Column_Number'img);
                  Put_Line (Quote ("  Column_Coordinate:",
                     Cell.Column_Coordinate.Value));

               when Image_Field =>
                  Put_Line (Quote ("  Image path", Cell.Image_Div.Path));

               when Preset_Field =>
                  Put_Line ("  Preset_ID:" & Cell.Preset_ID'img);
                  Put_Line (Quote ("  Preset_ID_Field:",
                     Cell.Preset_ID_Field.Value));
                  Put_Line ("  Preset_Set:" & Cell.Preset_Set'img);

               when Row_Field =>
                  Put_Line ("  Row_Number:" & Cell.Row_Number'img);
                  Put_Line (Quote ("  Row_Coordinate:",
                     Cell.Row_Coordinate.Value));

               when others =>
                  null;

            end case;
         end if;
      end Dump;

      ----------------------------------------------------------------
      overriding
      function Get_Cell (
         Column                  : in out Preset_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access is
      ----------------------------------------------------------------

      begin
         return Generic_Cell_Package.Cell_Class_Access'(
            Generic_Cell_Package.Cell_Class_Access (Column.Cell));
      end Get_Cell;

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
         Cell                 : in     Cell_Class_Access;
         Coordinate           : in     Row_Type) is
      -------------------------------------------------------------

         Connection_Data      : Base.Connection_Data_Type renames
                                 Base.Connection_Data_Type (
                                    Ada_Lib.GNOGA.Get_Connection_Data.all);
         State                : Configuration.Camera.State.State_Type renames
                                 Connection_Data.State;
      begin
         Row_Package.Update (State, Cell.Row_Number, Cell.Row_Coordinate,
            Coordinate);
      end Row_Package_Update;

      ----------------------------------------------------------------
      procedure Select_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event             : in     Gnoga.Gui.Base.Mouse_Event_Record) is
      ----------------------------------------------------------------

         Connection_Data         : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
         Cell                    : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Object.Parent);
      begin
         Log_In (Debug);
         if Mouse_Event.Left_Button then
            Connection_Data.Camera.Set_Preset (Cell.Preset_ID);
         end if;
         Log_Out (Debug);
      end Select_Handler;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      pragma Unreferenced (Cell, Update_Parameter);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Not_Implemented;
--       Cell.Preset_ID := Preset_ID;
--       Cell.Preset_ID_Field.Value (Preset_ID'img);
         Log_Out (Debug);
      end Update_Cell;

      ----------------------------------------------------------------
      procedure Update_Handler (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      ----------------------------------------------------------------

         Connection_Data         : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Ada_Lib.GNOGA.Get_Connection_Data.all);
         Configured_Card         : Configured_Card_Type renames
                                    Connection_Data.Get_Configured_Card.all;
         Cell                    : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Object.Parent);
         Configuration           : constant Configuration_Type'class :=
                                    Global_Camera_Setup.
                                       Get_Configuration (Cell.Configuration_ID);
         ID                      : constant String :=
                                    Configured_Card.ID;
         Preset                  : constant Preset_Type'class :=
                                    Global_Camera_Setup.Get_Preset (
                                       Configuration.Preset_ID);
         State                   : Standard.Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;

         generic

            Coordinate_Name      : String;
            type Coordinate_Type is range <>;
            type Other_Coordinate_Type
                                 is range <>;
            with function Cell_Coordinate (
               Cell              : in     Cell_Class_Access
            ) return String;
            with function Cell_Number (
               Cell              : in     Cell_Class_Access
            ) return Coordinate_Type;
            with function Generic_Check_Image (
               New_Coordinate    : in     Coordinate_Type;
               Other_Coordinate  : in     Other_Coordinate_Type
            ) return String;
            with function Preset_Other_Coordinate (
               Preset            : in     Preset_Type'class
            ) return Other_Coordinate_Type;
            with function Generic_Preset_ID (
               New_Coordinate    : in     Coordinate_Type;
               Other_Coordinate  : in     Other_Coordinate_Type
            ) return Preset_ID_Type;
            with procedure Package_Update (
               Cell              : in     Cell_Class_Access;
               Coordinate        : in     Coordinate_Type);

         procedure Update_Coordinate
         with Pre => Ada_Lib.GNOGA.Has_Connection_Data;

         -------------------------------------------------------------
         function Column_Cell_Coordinate(
            Cell                 : in     Cell_Class_Access
         ) return String is
         -------------------------------------------------------------

         begin
            return Cell.Column_Coordinate.Value;
         end Column_Cell_Coordinate;

         -------------------------------------------------------------
         function Column_Cell_Number (
            Cell                 : in     Cell_Class_Access
         ) return Column_Type is
         -------------------------------------------------------------

         begin
            return Cell.Column_Number;
         end Column_Cell_Number;

         -------------------------------------------------------------
         procedure Update_Coordinate is
         -------------------------------------------------------------

         begin
            Log_Here (Debug, "coordinate " & Coordinate_Name &
               Quote (" coordinate value ", Cell_Coordinate (Cell)) &
               " cell number" & Cell_NUmber (Cell)'img);
            declare
               Raw_Coordinate    : constant Integer :=
                                    Integer'Value (Cell_Coordinate (Cell));
            begin
               Log_Here (Debug,
                  " new " & Coordinate_Name & Raw_Coordinate'img &
                  " old " & Coordinate_Name & Cell_Number (Cell)'img &
                  " column" & Preset_Other_Coordinate (Preset)'img);

               if Row_Package.Valid (State, Raw_Coordinate) then
                  declare
--                   Current_Preset_Cell : constant Cell_Class_Access := Cell_Class_Access (
--                                  Configured_Card.Get_Cell (Preset_Field,
--                                     Cell.Configuration_ID));
                     Image_Cell  : constant Cell_Class_Access := Cell_Class_Access (
                                    Configured_Card.Get_Cell (Image_Field,
                                    Cell.Configuration_ID));
                     New_Coordinate
                                 : constant Coordinate_Type :=
                                    Coordinate_Type (Raw_Coordinate);
                     New_Preset_ID
                                 : constant Preset_ID_Type :=
                                    Generic_Preset_ID (New_Coordinate,
                                       Preset_Other_Coordinate (Preset));
                     Preset_Cell : constant Cell_Class_Access := Cell_Class_Access (
                                    Configured_Card.Get_Cell (Preset_Field,
                                       Cell.Configuration_ID));
                  begin
                     Log_Here (Debug,
                        " new preset id" & New_Preset_ID'img &
                        " other coordinate" & Preset_Other_Coordinate (Preset)'img);

                     Package_Update (Cell, New_Coordinate);
                     if New_Preset_ID = Preset_Not_Set then
                        Log_Here (Debug);
                        Preset_Cell.Preset_ID := Preset_Not_Set;
                        Preset_Cell.Preset_ID_Field.Value ("");
                        Image_Cell.Image_Div.Image.URL_Source (Blank_Preset);
                        Image_Cell.Image_Div.Path.Construct (Blank_Preset);
                     else
                        Log_Here (Debug);
                        declare
                           Path  : constant String :=
                                    Generic_Check_Image (
                                       New_Coordinate    => New_Coordinate,
                                       Other_Coordinate  =>
                                          Preset_Other_Coordinate (Preset));
                        begin
                           Log_Here (Debug, Quote ("image", Path));
                           Global_Camera_Setup.Update_Configuration (
                              Cell.Configuration_ID, New_Preset_ID);
                           Preset_ID_Package.Update (
                              State, Preset_Cell.Preset_ID,
                              Preset_Cell.Preset_ID_Field, New_Preset_ID);

                           Image_Cell.Image_Div.Image.URL_Source (Path);
                           Image_Cell.Image_Div.Path.Construct (Path);
                        end;
                     end if;
                  end;
               else
                  Log_Here (Debug);
                  -- put text of row number back in field
                  Package_Update (Cell, Cell_Number (Cell));
               end if;
            end;
         end Update_Coordinate;

         -------------------------------------------------------------
         function Row_Cell_Coordinate(
            Cell                 : in     Cell_Class_Access
         ) return String is
         -------------------------------------------------------------

         begin
            return Cell.Row_Coordinate.Value;
         end Row_Cell_Coordinate;

         -------------------------------------------------------------
         function Row_Cell_Number (
            Cell                 : in     Cell_Class_Access
         ) return Row_Type is
         -------------------------------------------------------------

         begin
            return Cell.Row_Number;
         end Row_Cell_Number;

         -------------------------------------------------------------
         function Row_Check_Image (
            New_Coordinate    : in     Row_Type;
            Other_Coordinate  : in     Column_Type
         ) return String IS
         -------------------------------------------------------------

         begin
            return Image_Name (
               Row      => New_Coordinate,
               Column   => Other_Coordinate);
         end Row_Check_Image;

         -------------------------------------------------------------
         function Row_Preset_Other_Coordinate (
            Preset               : in     Preset_Type'class
         ) return Column_Type is
         -------------------------------------------------------------

         begin
            return Preset.Column;
         end Row_Preset_Other_Coordinate;

         -------------------------------------------------------------
         function Row_Preset_ID (
            Coordinate           : in     Row_Type;
            Other_Coordinate      : in     Column_Type
         ) return Preset_ID_Type is
         -------------------------------------------------------------

         begin
            return Global_Camera_Setup.Get_Preset_ID (
               Row => Coordinate,
               Column   => Other_Coordinate);
         end Row_Preset_ID;

         procedure Update_Row is new Update_Coordinate (
            Coordinate_Name         => "row",
            Coordinate_Type         => Row_Type,
            Other_Coordinate_Type   => Column_Type,
            Cell_Coordinate         => Row_Cell_Coordinate,
            Cell_Number             => Row_Cell_Number,
            Generic_Check_Image     => Row_Check_Image,
            Preset_Other_Coordinate => Row_Preset_Other_Coordinate,
            Generic_Preset_ID       => Row_Preset_ID,
            Package_Update          => Row_Package_Update);

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
            New_Coordinate    : in     Column_Type;
            Other_Coordinate  : in     Row_Type
         ) return String IS
         -------------------------------------------------------------

         begin
            return Image_Name (
               Column   => New_Coordinate,
               Row      => Other_Coordinate);
         end Column_Check_Image;

         -------------------------------------------------------------
         function Column_Preset_Other_Coordinate (
            Preset               : in     Preset_Type'class
         ) return Row_Type is
         -------------------------------------------------------------

         begin
            return Preset.Row;
         end Column_Preset_Other_Coordinate;

         -------------------------------------------------------------
         function Column_Preset_ID (
            Coordinate           : in     Column_Type;
            Other_Coordinate      : in     Row_Type
         ) return Preset_ID_Type is
         -------------------------------------------------------------

         begin
            return Global_Camera_Setup.Get_Preset_ID (
               Column   => Coordinate,
               Row      => Other_Coordinate);
         end Column_Preset_ID;

         procedure Update_Column is new Update_Coordinate (
            Coordinate_Name         => "column",
            Coordinate_Type         => Column_Type,
            Other_Coordinate_Type   => Row_Type,
            Cell_Coordinate         => Column_Cell_Coordinate,
            Cell_Number             => Column_Cell_Number,
            Generic_Check_Image     => Column_Check_Image,
            Preset_Other_Coordinate => Column_Preset_Other_Coordinate,
            Generic_Preset_ID       => Column_Preset_ID,
            Package_Update          => Column_Package_Update);

      begin -- Update_Handler
         Log_In (Debug, Quote ("update field column " & Cell.Column'img &
            " id", ID) & Quote (" object id", Object.ID) &
            " configuration id" & Cell.Configuration_ID'img);

         if Debug then
            Global_Camera_Setup.Dump;
         end if;

         Connection_Data.Reset_Update_Event;

         case Cell.Column is

            when Column_Field =>
               Update_Column;

            when Label_Field =>
               declare
                  New_Label      : constant String := Cell.Label.Value;

               begin
                  Log_Here (Debug, Quote ("label", New_Label));
                  Global_Camera_Setup.Update_Configuration (
                     Cell.Configuration_ID, New_Label);
               end;

            when Preset_Field =>
               Update_Preset_Cell (Configured_Card, Cell.Configuration_ID,
                  Cell.all);

            when Row_Field =>
               Update_Row;

            when others =>
               Log_Exception (Debug);
               raise Failed with "handler not implemmented for " &
                  Cell.Column'img & " at " & Here;

         end case;

         Connection_Data.Trigger_Update_Event;
         if Debug then
            Global_Camera_Setup.Dump;
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
         Configuration_ID           : in     Configuration_ID_Type;
         Preset_Cell                : in out Preset_Package.Cell_Type'class) is
      ----------------------------------------------------------------

         Connection_Data            : Base.Connection_Data_Type renames
                                       Base.Connection_Data_Type (
                                          Ada_Lib.GNOGA.Get_Connection_Data.all);
         Raw_Value                  : constant String :=
                                       Preset_Cell.Preset_ID_Field.Value;
         New_Preset_ID              : constant Preset_ID_Type :=
                                       (if Raw_Value'length = 0 then
                                          Preset_Not_Set
                                       else
                                          Preset_ID_Type'value (Raw_Value));
         State                      : Configuration.Camera.State.State_Type renames
                                       Connection_Data.State;
      begin
         Log_In (Debug, "Configuration_ID" & Configuration_ID'img &
            Quote (" raw preset value", Raw_Value) &
            " new preset id" & New_Preset_ID'img);
         if New_Preset_ID = Preset_Not_Set then -- put back the previous preset
            Log_Here (Debug);
            Preset_Cell.Preset_ID_Field.Value (Preset_Cell.Preset_ID'img);
         else
            Log_Here (Debug);
            declare
               Column_Cell       : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Configured_Card.Get_Cell (Column_Field,
                                       Configuration_ID));
               Image_Cell        : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Configured_Card.Get_Cell (Image_Field,
                                       Configuration_ID));
               Preset            : constant
                                    Preset_Type'class :=
                                       (if New_Preset_ID = Preset_Not_Set then
                                             Null_Preset
                                          else
                                             Global_Camera_Setup.Get_Preset (
                                                New_Preset_ID));
               Path              : constant String := Image_Name (
                                    Row      => Preset.Row,
                                    Column   => Preset.Column);
               Row_Cell          : constant Preset_Package.Cell_Class_Access :=
                                    Preset_Package.Cell_Class_Access (
                                       Configured_Card.Get_Cell (Row_Field,
                                       Configuration_ID));
            begin
               Log_Here (Debug, Quote ("image", Path));
               Global_Camera_Setup.Update_Configuration (
                  Configuration_ID, New_Preset_ID);
               Preset_ID_Package.Update (State, Preset_Cell.Preset_ID,
                  Preset_Cell.Preset_ID_Field, New_Preset_ID);

               Column_Package_Update (Column_Cell, Preset.Column);
               Image_Cell.Image_Div.Image.URL_Source (Path);
               Image_Cell.Image_Div.Path.Construct (Path);
               Row_Package_Update (Row_Cell, Preset.Row);
            end;
         end if;
      end Update_Preset_Cell;

   end Preset_Package;

end Widgets.Configured;

