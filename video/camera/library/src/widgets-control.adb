with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Commands;
with Camera.Configurations;
with Camera.Base;
with Camera.Main;
with Configuration.Camera.Setup;
with Configuration.Camera.State;
--with Camera.Configurations;
--with GNOGA_Ada_Lib;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
--with Gnoga.Gui.View;
with Widgets.Generic_Table;
--with Video.Lib;

package body Widgets.Control is

   Header_Column                 : constant := 0;
-- Max_Columns                   : constant := 10;
-- Max_Rows                      : constant := 10;

   type Control_Column_Index_Type
                                 is new Natural;

   package Generic_Cell_Package is new Widgets.Generic_Table.Cell_Package (
      Column_Index_Type => Control_Column_Index_Type,
      Row_Index_Type    => Row_Index_Type);

   package Control_Package is

--    type Header_Type           is new Gnoga.Gui.Element.Table.Table_Header_Type
--                                  with null record;

      type Cell_Type (
         Column                  : Control_Column_Index_Type) is new
                                    Generic_Cell_Package.Cell_Type with record
         case Column is

            when Control_Column_Index_Type'first =>
               Label                   : Gnoga.Gui.Element.Common.Div_Type;

            when others =>
               Image                   : Gnoga.Gui.Element.Common.IMG_Type;
               Preset                  : Camera.Preset_ID_Type;
               Preset_Text             : Gnoga.Gui.Element.Common.P_Type;
               Image_Path              : Ada_Lib.Strings.Unlimited.String_Type;

         end case;
      end record;

--    type Cell_Access           is access all Cell_Type;
--    type Cell_Class_Access     is access all Cell_Type'class;

      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                             Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type);

      overriding
      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here);

      procedure Image_Click_Handler (
         Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record);

      overriding
      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class);

--    overriding
--    procedure Verify_Cell (
--       Cell                    : in out Cell_Type;
--       Verify_Parameter        : in     Generic_Cell_Package.
--                                           Verify_Parameter_Type'class);

      type Control_Column_Type   is new Generic_Cell_Package.
                                    GNOGA_Column_Type with null record;
--       Cell                    : Cell_Class_Access;
--    end record;

      type Control_Column_Access is access Control_Column_Type;

      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Class_Access;
         Column_Index            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type);

      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

   end Control_Package;

   subtype Column_Labels_Access  is Generic_Cell_Package.
                                    Column_Labels_Access;

   function Create_Labels (
      Number_Columns             : in     Control_Column_Index_Type
   ) return Column_Labels_Access;

   package Generic_Package is new Widgets.Generic_Table.Generic_Table_Package (
      Allocate_Column        => Control_Package.Allocate_Column,
      Column_Header        => True,
      Column_Index_Type    => Control_Column_Index_Type,
      Create_Column_Labels => Create_Labels,
      Create_Form          => False,
--    Form_Field           => False,
--    Formal_Column_Class_Access
--                         => Generic_Cell_Package.GNOGA_Column_Class_Access,
      Generic_Cell_Package => Generic_Cell_Package,
      Generic_Widget_Type  => Generic_Cell_Package.Widget_Type,
      Header_Type          => Gnoga.Gui.Element.Table.Table_Heading_Type,
      On_Submit            => Control_Package.On_Submit'access,
      Row_Header           => True,
      Row_Index_Type       => Row_Index_Type);

   -- widget used to control the camera
   type Card_Type   is new Generic_Package.Widget_Type
                                    with null record;

-- overriding
-- procedure Verify_Widget (
--    Widget                     : in     Card_Type;
--    Verify_Parameter           : in     Widgets.Generic_Table.
--                                           Verify_Parameter_Class_Access);

   type Full_Control_Card_Type   is new Control_Card_Type with record
      Card                       : aliased Card_Type;
   end record;

-- type Full_Control_Card_Class_Access
--                               is access all Full_Control_Card_Type'class;

   overriding
   procedure Create (
      Control_Card               : in out Full_Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "");

   overriding
   function Get_Card (
      Card                       : in out Full_Control_Card_Type
   ) return Gnoga.Gui.View.Pointer_To_View_Base_Class;

-- overriding
-- procedure Verify_Widget (
--    Widget                     : in     Full_Control_Card_Type;
--    Verify_Parameter           : in     Widgets.Generic_Table.
--                                           Verify_Parameter_Class_Access);

   ----------------------------------------------------------------
   function Allocate_Control_Card
   return Control_Card_Class_Access is
   ----------------------------------------------------------------

   begin
      return new Full_Control_Card_Type;
   end Allocate_Control_Card;

   ----------------------------------------------------------------
   procedure Class_Name (
      Card                       : in     Control_Card_Type) is
   ----------------------------------------------------------------

   begin
not_implemented;
   end Class_Name;

   ----------------------------------------------------------------
   procedure Create (
      Control_Card               : in out Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "") is
   ----------------------------------------------------------------

   begin
not_implemented;
   end Create;

   ----------------------------------------------------------------
   overriding
   procedure Create (
      Control_Card               : in out Full_Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "") is
   ----------------------------------------------------------------

      Top_Name       : constant String := (if ID'length = 0 then
                            ""
                         else
                            ID & "_")
                         & Widget_Name;
      State_Pointer
            : constant Camera.Configurations.
               Camera_Configuration_State_Constant_Class_Access :=
                  Camera.Configurations.
                     Get_Read_Only_Camera_Configuration_State;
      State : Configuration.Camera.State.State_Type'class renames
               State_Pointer.all;
      Number_Columns
            : constant Control_Column_Index_Type :=
               Control_Column_Index_Type (State.Get_Number_Columns);
      Number_Rows
            : constant Configuration.Row_Type := State.Get_Number_Rows;
   begin
      Log_In (Debug, Quote ("ID", ID) & Quote (" Top_Name", Top_Name) &
         Quote (" Widget_Name", Widget_Name));
--    Gnoga.Gui.Element.Common.DIV_Type (Control_Card).Create (
--       Parent   => Parent,
--       Content  => Content,
--       ID       => Top_Name);
      Control_Card.Card.Create (
         Main_Window    => Main_Window,
         Parent         => Gnoga.Gui.View.View_Base_Type'Class (Parent),
         Parent_Form    => Null,
         Number_Columns => Number_Columns,
         Number_Rows    => Number_Rows,
         Name           => Top_Name & "_Control");

      Log_Out (Debug);
   end Create;

   -------------------------------------------------------------------
   function Create_Labels (
      Number_Columns             : in     Control_Column_Index_Type
   ) return Column_Labels_Access is
   -------------------------------------------------------------------

      Labels                     : constant Column_Labels_Access :=
                                    new Generic_Cell_Package.Column_Labels_Type (
                                       Control_Column_Index_Type'first ..
                                          Number_Columns);
   begin
      Log_In (Debug, "Number_Columns" & Number_Columns'img);
      for Column in Control_Column_Index_Type'first .. Number_Columns loop
         Labels (Column) := new String'((if Column = Header_Column then
               "Row"
            else
               Ada_Lib.Strings.Trim (Column'img)));
      end loop;
      Log_Out (Debug);
      return Labels;
   end Create_Labels;

   ----------------------------------------------------------------
   overriding
   function Get_Card (
      Card                       : in out Full_Control_Card_Type
   ) return Gnoga.Gui.View.Pointer_To_View_Base_Class is
   ----------------------------------------------------------------

   begin
      return Card.Card'unchecked_access;
   end Get_Card;

--   ----------------------------------------------------------------
--   procedure Verify_Widget (
--      Widget                     : in     Control_Card_Type;
--      Verify_Parameter           : in     Widgets.Generic_Table.
--                                             Verify_Parameter_Class_Access) is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--   end Verify_Widget;

--   ----------------------------------------------------------------
--   overriding
--   procedure Verify_Widget (
--      Widget                     : in     Full_Control_Card_Type;
--      Verify_Parameter           : in     Widgets.Generic_Table.
--                                             Verify_Parameter_Class_Access) is
--   ----------------------------------------------------------------
--
--   begin
--      Log_In (Debug);
--not_implemented;
--      Log_Out (Debug);
--   end Verify_Widget;

   -------------------------------------------------------------------
   package body Control_Package is

      ----------------------------------------------------------------
      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Class_Access;
         Column_Index            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type) is
      ----------------------------------------------------------------

         Local_Column            : constant Control_Column_Access :=
                                    new Control_Column_Type;
      begin
         Log_Here (Debug, "column" & Column_Index'img &
            " row" & Table_Row'img);
         Column := Generic_Cell_Package.GNOGA_Column_Class_Access (Local_Column);
         Local_Column.Cell := new Cell_Type (Column_Index);
      end Allocate_Column;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                        : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type) is
      pragma Unreferenced (Form);
      ----------------------------------------------------------------

         Name                    : constant String := Row.ID & "_" &
                                    Ada_Lib.Strings.Trim (Table_Row'img);

      begin
         Log_In (Debug, Quote ("name", Name) & " Row id " & Row.ID &
            " table column" & Table_Column'img);
         Cell.Create (Column, ID => Name & "_Cell_" &
            Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
            Ada_Lib.Strings.Trim (Table_Column'img));
         Cell.Class_Name (Configuration.Camera.Control_Cell_Style);

         if Table_Column = Header_Column then
            Cell.Label.Create (
               Parent   => Cell,
               Content  => Table_Row'img,
               ID       => Name & "Label");
         else
            declare
               Image             : Gnoga.Gui.Element.Common.IMG_Type
                                    renames Cell.Image;
               Preset_ID         : constant Camera.Preset_ID_Type :=
                                    Configuration.Camera.Setup.Global_Camera_Setup.
                                       Get_Preset_ID (Table_Row,
                                          Configuration.Column_Type (
                                             Table_Column));
               Preset_Text       :  Gnoga.Gui.Element.Common.P_Type
                                    renames Cell.Preset_Text;
               Image_Name        : constant String :=
                                    Configuration.Camera.State.Image_Name (
                                       Configuration.Column_Type (
                                          Table_Column),
                                       Table_Row);
            begin
               Log_Here (Debug, "row" & Table_Row'img &
                  " column" & Table_Column'img &
                  Quote (" image Name", Image_Name) &
                  " preset id " & Preset_ID.Get_ID'img &
                  " is set " & Preset_ID.Is_Set'img);

               if Image_Name'length > 0 then
                  Cell.Image_Path.Construct (Image_Name);
                  Cell.Preset := Preset_ID;

                  Image.Create (
                     Parent            => Cell,
                     URL_Source        => Cell.Image_Path.Coerce,
                     Alternative_Text  => Table_Row'img &
                                             Table_Column'img,
                     ID                => Name & "_Image_" &
                                          Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                          Ada_Lib.Strings.Trim (Table_Column'img));
                  Image.On_Mouse_Click_Handler (
                    Image_Click_Handler'Access);
               else
                  Image.Create (
                     Parent            => Cell,
                     URL_Source        => Configuration.Camera.Blank_Preset,
                     Alternative_Text  => Table_Row'img &
                                             Table_Column'img,
                     ID                => Name & "_Image_" &
                                          Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
                                          Ada_Lib.Strings.Trim (Table_Column'img));
               end if;

               Image.Class_Name (Configuration.Camera.Control_Image_Style);
               if not Preset_ID.Is_Set then -- /= Configuration.Camera.Get_Preset_Not_Set then
                  Preset_Text.Create (
                     Parent   => Cell,
                     Content  => "no preset"); -- Preset_ID.Get_ID'img);
                  Preset_Text.Class_Name (Configuration.Camera.Control_Text_Style);
               end if;
            end;
         end if;
         Log_Out (Debug);
      end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
Not_Implemented;
end Dump;

      ----------------------------------------------------------------
      procedure Image_Click_Handler (
         Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record) is
      ----------------------------------------------------------------

         Cell     : Cell_Type renames Cell_Type (Object.Parent.all);
         Window_Connection
                  : constant Standard.Camera.Main.
                     Window_Connection_Class_Access :=
                        Standard.Camera.Main.Window_Connection_Class_Access (
                           Object.Connection_Data);
         Camera   : Standard.Camera.Commands.Camera_Type'class renames
                     Window_Connection.Get_Camera.all;

      begin
         Log_In (Debug, "preset" & Cell.Preset'img &
            " message " & Mouse_Event.Message'img & " ID " & Object.ID);

         Camera.Set_Preset (Cell.Preset);

         Log_Out (Debug);
      exception
         when Fault : others =>
            Trace_Exception (Debug, Fault);
            Standard.Camera.Base.Report_Exception (
               Window_Connection.Get_Main_Window.all,
                  Fault, "call preset failed");

      end Image_Click_Handler;

      ----------------------------------------------------------------
      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Log_Out (Debug);
      end On_Submit;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         Log_Out (Debug);
      end Update_Cell;

   end Control_Package;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Widgets.Control;

