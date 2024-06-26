--with Ada_Lib.Directory;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Base;
--with Camera.Lib;
with Configuration.Camera.Setup;
with Configuration.Camera.State;
with GNOGA.ADA_LIB;
with Gnoga.Gui.View;
with Main;
--with Video.Lib;

package body Widgets.Control is

   ----------------------------------------------------------------
   procedure Create (
      Control_Card               : in out Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "") is
   ----------------------------------------------------------------

      Top_Name                   : constant String := (if ID'length = 0 then
                                       ""
                                    else
                                       ID & "_")
                                    & Widget_Name;
      State                      : Configuration.Camera.State.State_Type
                                    renames Configuration.Camera.State.
                                       Global_Camera_State.all;
      Number_Columns             : constant Control_Column_Index_Type :=
                                    Control_Column_Index_Type (
                                       State.Number_Columns);
      Number_Rows                : constant Configuration.Camera.Row_Type :=
                                       State.Number_Rows;
   begin
      Log_In (Debug, Quote ("ID", ID) & Quote (" Top_Name", Top_Name) &
         Quote (" Widget_Name", Widget_Name));
--    Gnoga.Gui.Element.Common.DIV_Type (Control_Card).Create (
--       Parent   => Parent,
--       Content  => Content,
--       ID       => Top_Name);
      Control_Card.Create (
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
   procedure Verify_Widget (
      Widget                     : in     Control_Card_Type;
      Verify_Parameter           : in     Generic_Package.
                                             Verify_Parameter_Class_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
not_implemented;
      Log_Out (Debug);
   end Verify_Widget;

   -------------------------------------------------------------------
   package body Control_Package is

      ----------------------------------------------------------------
      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type) is
      ----------------------------------------------------------------

         Local_Column            : constant Control_Column_Access :=
                                    new Control_Column_Type;
      begin
         Log_Here (Debug, "column" & Column_Index'img &
            " row" & Table_Row'img);
         Column := Generic_Cell_Package.Generic_Column_Class_Access (Local_Column);
         Local_Column.Cell := new Cell_Type (Column_Index);
      end Allocate_Column;

      ----------------------------------------------------------------
      overriding
      function Get_Cell (
         Column                  : in out Control_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access is
      ----------------------------------------------------------------

      begin
         return Generic_Cell_Package.Cell_Class_Access'(
            Generic_Cell_Package.Cell_Class_Access (Column.Cell));
      end Get_Cell;

      ----------------------------------------------------------------
      procedure Image_Click_Handler (
         Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
         Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record) is
      ----------------------------------------------------------------
         Connection_Data            : Base.Connection_Data_Type renames
                                       Base.Connection_Data_Type (
                                          Object.Connection_Data.all);
         Main_Data                  : Main.Main_Data_Type renames
                                        Connection_Data.Main_Data.all;
         Cell                       : Cell_Type renames Cell_Type (
                                       Object.Parent.all);

      begin
         Log_In (Debug, "preset" & Cell.Preset'img &
            " message " & Mouse_Event.Message'img & " ID " & Object.ID);

         Connection_Data.Camera.Set_Preset (Cell.Preset);

         Log_Out (Debug);
      exception
         when Fault : others =>
            Trace_Exception (Debug, Fault);
            GNOGA.Ada_Lib.Report_Exception (Main_Data.Main_Window.all,
               Fault, "call preset failed");

      end Image_Click_Handler;

      ----------------------------------------------------------------
      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                        : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Type'class;
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
               Preset_ID         : constant Configuration.Camera.Preset_ID_Type :=
                                    Configuration.Camera.Setup.Global_Camera_Setup.
                                       Get_Preset_ID (Table_Row,
                                          Configuration.Camera.Column_Type (
                                             Table_Column));
               Preset_Text       :  Gnoga.Gui.Element.Common.P_Type
                                    renames Cell.Preset_Text;
               Image_Name        : constant String :=
                                    Check_Image (
                                       Configuration.Camera.Column_Type (
                                          Table_Column),
                                       Table_Row);
            begin
               Log_Here (Debug, "row" & Table_Row'img &
                  " column" & Table_Column'img &
                  Quote (" image Name", Image_Name));

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
               if Preset_ID /= Configuration.Camera.Preset_Not_Set then
                  Preset_Text.Create (
                     Parent   => Cell,
                     Content  => Preset_ID'img);
                  Preset_Text.Class_Name (Control_Text_Style);
               end if;
            end;
         end if;
         Log_Out (Debug);
      end Create_Cell;

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

end Widgets.Control;

