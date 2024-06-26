--with Ada_Lib.Socket_IO;
with Ada_Lib.Strings; -- .Unlimited;
--with Camera.Lib;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;

package Widgets.Generic_Table is

   type Root_Cell_Type              is new Gnoga.Gui.Element.Common.DIV_Type
                                       with null record;
   type Root_Cell_Class_Access      is access all Root_Cell_Type'class;

   generic

      type Column_Index_Type        is ( <> );
      type Row_Index_Type           is ( <> );

   package Cell_Package is

      type Cell_Type                is abstract new Root_Cell_Type with null record;

      type Cell_Class_Access        is access all Cell_Type'class;

      type Generic_Column_Type      is abstract new Gnoga.Gui.Element.Table.
                                       Table_Column_Type with null record;
      type Generic_Column_Class_Access
                                    is access all Generic_Column_Type'class;
      function Get_Cell (
         Column                  : in out Generic_Column_Type
      ) return Cell_Class_Access is abstract;

      type Update_Parameter_Type is tagged null record;
--    type Verify_Parameter_Type is tagged null record;

      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Column_Type'class;
         Table_Column            : in     Column_Index_Type;
         Table_Row               : in     Row_Index_Type) is abstract;

      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Update_Parameter_Type'class) is abstract;

--    procedure Verify_Cell (
--       Cell                    : in out Cell_Type;
--       Verify_Parameter        : in     Verify_Parameter_Type'class) is abstract;

      type Column_Labels_Type       is array (Column_Index_Type range <>) of
                                       Ada_Lib.Strings.String_Constant_Access;

      type Column_Labels_Access     is access all Column_Labels_Type;

      procedure Create_Column (
         Column                     : in out Generic_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Row_Index_Type;
         Row_Index                  : in     Row_Index_Type;
         Column_Index               : in     Column_Index_Type;
         ID                         : in     String);

      type Widget_Type              is limited new
                                       Gnoga.Gui.Element.Common.DIV_Type
                                          with null record;

      function Create_Column (
         Widget                     : in out Widget_Type;
         Row                        : in     Row_Index_Type;
         Column                     : in     Column_Index_Type
      ) return Boolean;

      function Create_Row (
         Widget                     : in out Widget_Type;
         Row                        : in     Row_Index_Type
      ) return Boolean;

   end Cell_Package;

   generic

      type Column_Index_Type     is ( <> );
      type Row_Index_Type        is ( <> );
      type Header_Type           is new Gnoga.Gui.Element.Table.Table_Heading_Type
                                    with Private;

      with package Generic_Cell_Package is new Cell_Package (
         Column_Index_Type, Row_Index_Type);

--    type Formal_Column_Class_Access
--                               is access all Generic_Cell_Package.
--                                  Generic_Column_Type;

      type Generic_Widget_Type   is new Generic_Cell_Package.Widget_Type
                                    with private;

      with procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Column_Index_Type;
         Table_Row               : in     Row_Index_Type);

      with function Create_Column_Labels (
         Number_Columns          : in     Column_Index_Type
      ) return Generic_Cell_Package.Column_Labels_Access;

      Column_Header              : in     Boolean;
      Create_Form                : in     Boolean;
--    Form_Field                 : in     Boolean;
      On_Submit                  : in     Gnoga.Gui.Base.Action_Event;
      Row_Header                 : in     Boolean;

   package Generic_Table_Package is

--    type Cell_Type             is abstract new Generic_Cell_Package.Cell_Type
--                                  with null record;
--    type Cell_Access           is access all Cell_Type;
--    type Cell_Class_Access     is access all Cell_Type'class;

      type Row_Type              is new Gnoga.Gui.Element.Table.Table_Row_Type
                                    with private;
      type Row_Class_Access      is access all Row_Type'class;

      procedure Update_Row (
         Row                     : in     Row_Type);

      type Table_Type            is new Gnoga.Gui.Element.Table.Table_Type with
                                    private;

      type Verify_Parameter_Type is tagged;
      type Verify_Parameter_Class_Access
                                 is access all Verify_Parameter_Type'class;

      type Widget_Type           is abstract new Generic_Widget_Type
                                    with private;

      type Widget_Access         is access all Widget_Type;
      type Widget_Class_Access   is access all Widget_Type'class;

      procedure Create (
         Widget                  : in out Widget_Type;
         Main_Window             : in out Gnoga.GUI.Window.Window_Type'Class;
         Parent                  : in out Gnoga.Gui.View.View_Base_Type'Class;
         Parent_Form             : in     Gnoga.Gui.Element.Form.
                                             Pointer_To_Botton_Class;
         Number_Columns          : in     Column_Index_Type;
         Number_Rows             : in     Row_Index_Type;
         Name                    : in     String);

      function Get_Accept_Button (
         Widget                     : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class;

      function Get_Cancel_Button (
         Widget                     : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class;

      function Get_Form (
         Widget                  : in out Widget_Type
      ) return Gnoga.Gui.Element.Form.Pointer_To_Botton_Class;

      function Get_Cell (
         Widget                  : in     Widget_Type;
         Column_Index            : in     Column_Index_Type;
         Row_Index               : in     Row_Index_Type
      ) return Generic_Cell_Package.Cell_Class_Access;

      function Get_Cell (     -- from form field
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class
      ) return Generic_Cell_Package.Cell_Class_Access;

      function Get_Column (
         Row                     : in     Row_Type;
         Column_Index            : in     Column_Index_Type
      ) return Generic_Cell_Package.Generic_Column_Class_Access;

      function Get_Row (
         Widget                  : in     Widget_Type;
         Row_Index               : in     Row_Index_Type
      ) return Row_Class_Access;

      procedure Verify_Widget (
         Widget                  : in     Widget_Type;
         Verify_Parameter        : in     Verify_Parameter_Class_Access) is abstract;

      function Get_Widget (    -- from form field
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class
      ) return Widget_Class_Access;

      type Verify_Parameter_Type is abstract tagged null record;

      procedure Verify_Widget (
         Verify_Parameter        : in     Verify_Parameter_Type;
         Widget                  : in     Widget_Type'class) is abstract;

   private

      type Columns_Type          is array (Column_Index_Type range <>) of
                                    Generic_Cell_Package.
                                       Generic_Column_Class_Access;
      type Columns_Access        is access Columns_Type;

      type Headers_Type          is array (Column_Index_Type range <>) of
                                    Header_Type;

      type Headers_Access        is access Headers_Type;

      type Row_Type              is new Gnoga.Gui.Element.Table.Table_Row_Type
                                    with record
         Columns                 : Columns_Access := Null;
--       Header                  : Gnoga.Gui.Element.Common.DIV_Type;
      end record;

      type Rows_Type             is array (Row_Index_Type range <>) of aliased Row_Type;

      type Rows_Access           is access Rows_Type;

      type Table_Type            is new Gnoga.Gui.Element.Table.Table_Type
                                    with record
         Accept_Button           : aliased Gnoga.Gui.Element.Form.
                                    Submit_Button_Type;
         Cancel_Button           : aliased Gnoga.Gui.Element.Form.
                                    Submit_Button_Type;
--       Header_Column           : Header_Type;
         Header_Row              : Gnoga.Gui.Element.Table.Table_Row_Type;
         Headers                 : Headers_Access;
         Rows                    : Rows_Access := Null;
      end record;

      type Grid_Type             is new Gnoga.Gui.Element.Common.DIV_Type
                                    with record
         Form                    : aliased Gnoga.Gui.Element.Form.Form_Type;
         Table                   : Table_Type;
      end record;

      type Widget_Type           is abstract new Generic_Widget_Type with record
           Grid                  : Grid_Type;
      end record;

--    procedure Submit_Handler (
--       Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

   end Generic_Table_Package;

   Debug                         : Boolean := False;

end Widgets.Generic_Table;

