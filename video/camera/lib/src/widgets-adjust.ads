--with Ada_Lib.Strings.Unlimited;
--with ADA_LIB.Trace;
--with Configuration;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;
with Widgets.Generic_Table;

package Widgets.Adjust is

   Failed                        : exception;

   Debug                         : aliased Boolean := False;
   Widget_Name                   : constant String := "Adjust_Card";

   type Cell_Type                is new Gnoga.Gui.Element.Common.DIV_Type
                                    with null record;

   type Outer_Column_Index_Type -- need to be on order columns created
                                 is (Left_Column, Center_Column, Right_Column);
   type Outer_Row_Index_Type     is (Top_Row, Center_Row, Bottom_Row);

   package Generic_Cell_Package
                              is new Widgets.Generic_Table.Cell_Package (
                                 Outer_Column_Index_Type,
                                 Outer_Row_Index_Type);

   package Outer_Package is

      type Slider_Type           is new Gnoga.Gui.Element.Common.Meter_Type with
                                    null record;

      overriding
      procedure Create (
         Meter                   : in out Slider_Type;
         Parent                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Value                   : in     Integer := 0;
         High                    : in     Integer := 100;
         Low                     : in     Integer := 0;
         Maximum                 : in     Integer := 100;
         Minimum                 : in     Integer := 0;
         Optimum                 : in     Integer := 50;
         ID                      : in     String := "");

      procedure Create (
         Element                 : in out Gnoga.Gui.Element.Form.Text_Type;
         Form                    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Value                   : in     String := "";
         Name                    : in     String := "";
         ID                      : in     String := "");

      type Cell_Type (
         Column                  : Outer_Column_Index_Type) is new
                                    Generic_Cell_Package.Cell_Type with record
         case Column is

            when Left_Column =>
               Vertical_Slider_Blank
                                 : Gnoga.Gui.Element.Common.Div_Type;
               Vertical_Slider_Solid
                                 : Gnoga.Gui.Element.Common.Div_Type;

            when Center_Column =>
               Center_Box        : Gnoga.Gui.Element.Common.DIV_Type;
               Horizontal_Slider : Gnoga.Gui.Element.Common.Progress_Bar_Type;

            when Right_Column =>
               null;

         end case;
      end record;

      type Cell_Access           is access all Cell_Type;
      type Cell_Class_Access     is access all Cell_Type'class;

      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                        : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Type'class;
         Table_Column            : in     Outer_Column_Index_Type;
         Table_Row               : in     Outer_Row_Index_Type);

      overriding
      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class);

      type Outer_Column_Type is new
                                    Generic_Cell_Package.
                                       Generic_Column_Type with record
         Cell                    : Cell_Class_Access := Null;
      end record;

      type Outer_Column_Access is access all Outer_Column_Type;

      overriding
      function Get_Cell (
         Column                  : in out Outer_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access;

      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Outer_Column_Index_Type;
         Table_Row               : in     Outer_Row_Index_Type);

      overriding
      procedure Create_Column (
         Column                     : in out Outer_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Outer_Row_Index_Type;
         Row_Index                  : in     Outer_Row_Index_Type;
         Column_Index               : in     Outer_Column_Index_Type;
         ID                         : in     String);

      type Widget_Type           is new Generic_Cell_Package.Widget_Type
                                    with null record;

      overriding
      function Create_Column (
         Widget                  : in out Widget_Type;
         Row                     : in     Outer_Row_Index_Type;
         Column                  : in     Outer_Column_Index_Type
      ) return Boolean;

      overriding
      function Create_Row (
         Widget                  : in out Widget_Type;
         Row                     : in     Outer_Row_Index_Type
      ) return Boolean;

      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

   end Outer_Package;

   subtype Cell_Class_Access     is Outer_Package.Cell_Class_Access;

   subtype Column_Labels_Type    is Generic_Cell_Package.
                                    Column_Labels_Type;

   subtype Column_Labels_Access  is Generic_Cell_Package.
                                    Column_Labels_Access;

   function Create_Labels (
      Number_Columns             : in        Outer_Column_Index_Type
   ) return Column_Labels_Access;

   package Adjust_Package is new Widgets.    -- row for each preset
         Generic_Table.Generic_Table_Package (        -- 1st column description
      Allocate_Column        => Outer_Package.Allocate_Column,
      Column_Header        => True,
      Column_Index_Type    => Outer_Column_Index_Type,
      Create_Column_Labels => Create_Labels,
      Create_Form          => True,
--    Form_Field           => False,
--    Formal_Column_Type   => Outer_Package.Outer_Column_Type,
      Generic_Cell_Package => Generic_Cell_Package,
      Generic_Widget_Type  => Outer_Package.Widget_Type,
      Header_Type          => Gnoga.Gui.Element.Table.Table_Heading_Type,
      On_Submit            => Outer_Package.On_Submit'access,
      Row_Header           => True,
      Row_Index_Type       => Outer_Row_Index_Type);

   type Adjust_Card_Type
                           is new Adjust_Package.Widget_Type with
                              null record;
   type Adjust_Card_Class_Access
                           is access all Adjust_Card_Type;
   procedure Create (
      Adjust_Card                : in out Adjust_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.View.View_Base_Type'Class);

   function Get_Cell (
      Adjust_Card                : in     Adjust_Card_Type;
      Column_Index               : in     Outer_Column_Index_Type;
      Row_Index                  : in     Outer_Row_Index_Type
   ) return Cell_Class_Access;

   overriding
   procedure Verify_Widget (
      Widget                     : in     Adjust_Card_Type;
      Verify_Parameter           : in     Adjust_Package.
                                             Verify_Parameter_Class_Access);

-- type Verify_Parameter_Type    is new Adjust_Package.Verify_Parameter_Type with
--                                  null record;
--
-- type Verify_Parameter_Class_Access
--                               is access all Verify_Parameter_Type'class;
--
-- procedure Verify_Widget (
--    Widget                     : in     Adjust_Card_Type;
--    Verify_Parameter           : in     Verify_Parameter_Class_Access);

end Widgets.Adjust;



