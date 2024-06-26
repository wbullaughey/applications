with Ada_Lib.Strings; use Ada_Lib.Strings;
with ADA_LIB.Trace; use ADA_LIB.Trace;

package body Widgets.Generic_Table is

   package body Cell_Package is

      ----------------------------------------------------------------
      procedure Create_Column (
         Column                     : in out Generic_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Row_Index_Type;
         Row_Index                  : in     Row_Index_Type;
         Column_Index               : in     Column_Index_Type;
         ID                         : in     String) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug, "row " & Row_Index'img & " column " & Column_Index'img &
            " id " & ID);
         Gnoga.Gui.Element.Table.Table_Column_Type (Column).Create (
            Column_Span    => 1,
            Content        => "",
            ID             => ID,
            Row            => Row,
            Row_Span       => 1);

         Log_Out (Debug);
      end Create_Column;

      ----------------------------------------------------------------
      function Create_Column (
         Widget                     : in out Widget_Type;
         Row                        : in     Row_Index_Type;
         Column                     : in     Column_Index_Type
      ) return Boolean is
      pragma Unreferenced (Widget);
      ----------------------------------------------------------------

      begin
         return Log_Here (True, Debug, "row" & Row'img & " column" & Column'img);
      end Create_Column;

      ----------------------------------------------------------------
      function Create_Row (
         Widget                     : in out Widget_Type;
         Row                        : in     Row_Index_Type
      ) return Boolean is
      pragma Unreferenced (Row, Widget);
      ----------------------------------------------------------------

      begin
         Log_Here (Debug);
         return True;
      end Create_Row;

   end Cell_Package;

   package body Generic_Table_Package is

     ----------------------------------------------------------------
      procedure Create (
         Widget                     : in out Widget_Type;
         Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
         Parent                     : in out Gnoga.Gui.View.View_Base_Type'Class;
         Parent_Form                : in     Gnoga.Gui.Element.Form.
                                             Pointer_To_Botton_Class;
         Number_Columns             : in     Column_Index_Type;
         Number_Rows                : in     Row_Index_Type;
         Name                       : in     String) is
      ----------------------------------------------------------------

         Column_Labels              : constant Generic_Cell_Package.
                                       Column_Labels_Access :=
                                          Create_Column_Labels (Number_Columns);
         Grid                       : Grid_Type renames Widget.Grid;
         Form                       : Gnoga.Gui.Element.Form.Form_Type
                                       renames Grid.Form;
         Table                      : Table_Type renames Grid.Table;

      begin
         Log_In (Debug, "number rows" & Number_Rows'img & " form " & Create_Form'img &
            " columns " & Number_Columns'img & Quote (" name", Name) &
            "row header " & Row_Header'img);
         Table.Rows := new Rows_Type (Row_Index_Type'first .. Number_Rows);
         Widget.Create (Parent, ID => Name);
         Grid.Create (Widget, ID => Name & "-Grid");
         Table.Create (Grid, ID => Name & "-Table");
         if Create_Form then
            Form.Create (Table, Name & " form");
         end if;

         if Column_Header then
            Table.Header_Row.Create (Table, ID => Name & "-Header_Row");
            Table.Headers := new Headers_Type (Column_Index_Type'first .. Number_Columns);

            for Column_Index in Column_Index_Type'first .. Number_Columns loop
               declare
                  Header         : Header_Type renames Table.Headers (Column_Index);
                  ID             : constant String := Name &
                                    "-Header_Column-" & Trim (Column_Index'img);
                  Label          : constant String :=
                                    Column_Labels (Column_Index).all;

               begin
                  Log_Here (Debug, "column " & Column_Index'img &
                     Quote (" label", Label) &
                     Quote (" header row ID", ID));
                  Header.Create (
                     Content  => Label,
                     ID    => ID,
                     Row   => Table.Header_Row,
                     Row_Span => 1);
               end;
            end loop;
         end if;

         Table.Rows := new Rows_Type (Row_Index_Type'first .. Number_Rows);
         for Row_Index in Row_Index_Type'first .. Number_Rows loop
            declare
               Row               : Row_Type renames Table.Rows (Row_Index);

            begin
               Log_Here (Debug, "row " & Row_Index'img);
               Row.Create (Table, ID => Name & "-" &Trim (Row_Index'img));
               Row.Columns := new Columns_Type (Column_Index_Type'first ..
                  Number_Columns);

               for Column_Index in Row.Columns'range loop
                  Log_Here (Debug, Name & " column " & Column_Index'img &
                     " number columns" & Number_Columns'img);
                  declare
                     Column      : Generic_Cell_Package.Generic_Column_Class_Access
                                    renames Row.Columns (Column_Index);
                  begin
                     Allocate_Column (Column, Column_Index, Row_Index);

                     declare
                        Cell     : constant Generic_Cell_Package.Cell_Class_Access :=
                                    Column.Get_Cell;
                        ID       : constant String :=  Name & "-" &
                                    Trim (Row_Index'img) & "-"  &
                                    Trim (Column_Index'img);
                     begin
                        Log_Here (Debug, "row " & Row_Index'img &
                           " column " & Column_Index'img &
                           Quote (" id",  ID) &
                           " rowspan" & Row_Index_Type'pos (Number_Rows)'img &
                           " column class " & Tag_Name (Column'tag));
                        Column.Create_Column (
                           Column_Index   => Column_Index,
                           ID             => ID,
                           Number_Rows    => Number_Rows,
                           Row            => Row,
                           Row_Index      => Row_Index);
                        Cell.Create_Cell (Grid.Form'unchecked_access,
                           Row, Column.all, Column_Index, Row_Index);
                     end;
                  end;
               end loop;
            end;
         end loop;

         Log_Here (Debug, "Create_Form " & Create_Form'img);
         if Create_Form then
            declare
               Accept_Button     : Gnoga.Gui.Element.Form.Submit_Button_Type renames
                                    Table.Accept_Button;
               Accept_Button_Name: constant String := "Accept";
               Cancel_Button     : Gnoga.Gui.Element.Form.Submit_Button_Type renames
                                    Table.Cancel_Button;
               Cancel_Button_Name: constant String := "Cancel";

            begin
               Accept_Button.Create (Form,
                  ID       => Accept_Button_Name,
                  Name     => Accept_Button_Name,
                  Value    => "Accept");
               Cancel_Button.Create (Form,
                  ID       => Cancel_Button_Name,
                  Name     => Cancel_Button_Name,
                  Value    => "Cancel");
               Form.On_Submit_Handler (On_Submit);
            end;
         end if;

         Log_Out (Debug);
--pause;

      exception
         when Fault : others =>
            Trace_Exception (Debug, Fault, Here);
            raise;

      end Create;

      ----------------------------------------------------------------
      function Get_Accept_Button (
         Widget                     : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class is
      ----------------------------------------------------------------

      begin
         return Widget.Grid.Table.Accept_Button'unchecked_access;
      end Get_Accept_Button;

      ----------------------------------------------------------------
      function Get_Cancel_Button (
         Widget                     : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class is
      ----------------------------------------------------------------

      begin
         return Widget.Grid.Table.Cancel_Button'unchecked_access;
      end Get_Cancel_Button;

      ----------------------------------------------------------------
      function Get_Cell (     -- from form field
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class
      ) return Generic_Cell_Package.Cell_Class_Access is
      ----------------------------------------------------------------

      begin
         Log_Here (Debug, "object tag " & Tag_Name (Object'tag));
         return Generic_Cell_Package.Cell_Class_Access (Object.Parent);
      end Get_Cell;

      ----------------------------------------------------------------
      function Get_Cell (
         Widget                  : in     Widget_Type;
         Column_Index            : in     Column_Index_Type;
         Row_Index               : in     Row_Index_Type
      ) return Generic_Cell_Package.Cell_Class_Access is
      ----------------------------------------------------------------

         Row                     : Row_Type'class renames Widget.Get_Row (
                                    Row_Index).all;
         Column                  : constant Generic_Cell_Package.
                                    Generic_Column_Class_Access :=
                                       Row.Columns (Column_Index);
         Cell                    : constant Generic_Cell_Package.Cell_Class_Access :=
                                    Column.Get_Cell;

      begin
         return Cell;
      end Get_Cell;

      ----------------------------------------------------------------
      function Get_Column (
         Row                     : in     Row_Type;
         Column_Index            : in     Column_Index_Type
      ) return Generic_Cell_Package.Generic_Column_Class_Access is
      ----------------------------------------------------------------

      begin
         return Row.Columns (Column_Index);
      end Get_Column;

      ----------------------------------------------------------------
      function Get_Form (
         Widget                  : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Botton_Class is
      ----------------------------------------------------------------

      begin
         return Widget.Grid.Form'unchecked_access;
      end Get_Form;

      ----------------------------------------------------------------
      function Get_Row (
         Widget                  : in     Widget_Type;
         Row_Index               : in     Row_Index_Type
      ) return Row_Class_Access is
      ----------------------------------------------------------------

      begin
         return Widget.Grid.Table.Rows (Row_Index)'access;
      end Get_Row;

      ----------------------------------------------------------------
      function Get_Widget (     -- from form field
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class
      ) return Widget_Class_Access is
      ----------------------------------------------------------------

      begin
         Log_Here (Debug, "object tag " & Tag_Name (Object'tag));

         declare
            Cell                 : constant Generic_Cell_Package.
                                    Cell_Class_Access := Get_Cell (Object);
            Column               : constant Generic_Cell_Package.
                                    Generic_Column_Class_Access :=
                                       Generic_Cell_Package.
                                          Generic_Column_Class_Access (
                                             Cell.Parent);
            Row                  : Row_Type renames Row_Type (Column.Parent.all);
            Table                : Table_Type renames Table_Type (Row.Parent.all);
            Grid                 : Grid_Type renames Grid_Type (Table.Parent.all);
            Widget               : Widget_Type renames Widget_Type (Grid.Parent.all);

         begin
            return Widget_Class_Access'(Widget'access);
         end;

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

      end Get_Widget;

      ----------------------------------------------------------------
      procedure Update_Row (
         Row                     : in     Row_Type) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Log_OUt (Debug);
      end Update_Row;

--    ----------------------------------------------------------------
--    procedure Verify_Widget (
--       Widget                  : in     Widget_Type;
--       Verify_Parameter        : in     Verify_Parameter_Type'class) is
--    ----------------------------------------------------------------
--
--    begin
--       Not_Implemented;
--    end Verify_Widget;

   end Generic_Table_Package;

end Widgets.Generic_Table;

