--with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited;
with Camera;
with Configuration.Camera; use Configuration.Camera;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
--with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.Element.Form;
--with Gnoga.Gui.View;
with Gnoga.GUI.Window;
--with Video.Lib;
with Widgets.Generic_Table;

package Widgets.Control is

   Failed                        : exception;

   Debug                         : aliased Boolean := False;
   Header_Column                 : constant := 0;
   Max_Columns                   : constant := 10;
   Max_Rows                      : constant := 10;
   Widget_Name                   : constant String := "Control_Card";

-- type Cell_Type                is new Widgets.Generic_Table.
--                                  Root_Cell_Type with null record;
-- type Cell_class_Access        is access all Cell_Type'class;
--
   type Control_Column_Index_Type
                                 is new Natural;

   package Generic_Cell_Package is new Widgets.Generic_Table.Cell_Package (
      Column_Index_Type => Control_Column_Index_Type,
      Row_Index_Type    => Row_Index_Type);

   package Control_Package is

      type Header_Type           is new Gnoga.Gui.Element.Table.Table_Header_Type
                                    with null record;

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

      type Cell_Access           is access all Cell_Type;
      type Cell_Class_Access     is access all Cell_Type'class;

      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                             Pointer_To_Botton_Class;
         Row                     : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Type'class;
         Table_Column            : in     Control_Column_Index_Type;
         Table_Row               : in     Row_Index_Type);

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
                                    Generic_Column_Type with record
         Cell                    : Cell_Class_Access;
      end record;

      type Control_Column_Access is access Control_Column_Type;

      overriding
      function Get_Cell (
         Column                  : in out Control_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access;

      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
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
--                         => Generic_Cell_Package.Generic_Column_Class_Access,
      Generic_Cell_Package => Generic_Cell_Package,
      Generic_Widget_Type  => Generic_Cell_Package.Widget_Type,
      Header_Type          => Gnoga.Gui.Element.Table.Table_Heading_Type,
      On_Submit            => Control_Package.On_Submit'access,
      Row_Header           => True,
      Row_Index_Type       => Row_Index_Type);

   type Control_Card_Type        is new Generic_Package.Widget_Type
                                    with null record;

   type Control_Card_Access      is access all Control_Card_Type;
   type Control_Card_Class_Access
                                 is access all Control_Card_Type'class;

   procedure Create (
      Control_Card               : in out Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "");

   overriding
   procedure Verify_Widget (
      Widget                     : in     Control_Card_Type;
      Verify_Parameter           : in     Generic_Package.
                                             Verify_Parameter_Class_Access);
end Widgets.Control;

