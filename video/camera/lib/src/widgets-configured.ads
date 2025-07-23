--with Ada_Lib.GNOGA;
with Ada_Lib.Strings.Unlimited;
with ADA_LIB.Trace;
with Camera;
--with Configuration.Camera.State;
with GNOGA_Ada_Lib;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;
with Widgets.Control;
with Widgets.Generic_Table;

package Widgets.Configured is

-- use type Configuration.Camera.State.State_Access;

   Failed                        : exception;

   Debug                         : aliased Boolean := False;
   Widget_Name                   : constant String := "Configured_Card";

   type Cell_Type                is new Gnoga.Gui.Element.Common.DIV_Type
                                    with null record;

   type Preset_Column_Index_Type -- need to be on order columns created
                              is (Row_Header, Label_Field, Preset_Field,
                               Column_Field, Row_Field, Image_Field, Control_Field);
   subtype Preset_Row_Index_Type
                              is Configuration.Camera.Configuration_ID_Type;

   package Generic_Cell_Package
                              is new Widgets.Generic_Table.Cell_Package (
                                 Preset_Column_Index_Type,
                                 Configuration.Camera.Configuration_ID_Type);

   package Preset_Package is

      procedure Create (
         Element                 : in out Gnoga.Gui.Element.Form.Text_Type;
         Form                    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Value                   : in     String := "";
         Name                    : in     String := "";
         ID                      : in     String := "");

      type Image_Div_Type        is new Gnoga.Gui.View.View_Base_Type with record
         Image                   : Gnoga.Gui.Element.Common.IMG_Type;
         Path                    : Ada_Lib.Strings.Unlimited.String_Type;
      end record;

      type Cell_Type (
         Column                  : Preset_Column_Index_Type) is new
                                    Generic_Cell_Package.Cell_Type with record
         Configuration_ID        : Configuration.Camera.Configuration_ID_Type;

         case Column is

            when Column_Field =>
               Column_Coordinate : Gnoga.Gui.Element.Form.Number_Type;
               Column_Number     : Configuration.Camera.Column_Type :=
                                    Configuration.Camera.Column_Not_Set;

            when Control_Field =>
               Control_Table     : Widgets.Control.Control_Card_Type;

            when Image_Field =>
               Image_Div         : Image_Div_Type;

            when Label_Field =>
               Label             : Gnoga.Gui.Element.Form.Text_Type;

            when Preset_Field =>
               Preset_ID         : Camera.Preset_ID_Type;
               Preset_ID_Field   : Gnoga.Gui.Element.Form.Number_Type;
               Preset_Set        : Boolean := False;

            when Row_Field =>
               Row_Coordinate    : Gnoga.Gui.Element.Form.Number_Type;
               Row_Number        : Configuration.Camera.Row_Type :=
                                    Configuration.Camera.Row_Not_Set;

            when Row_Header =>
               Button            : Gnoga.Gui.Element.Common.Button_Type;

         end case;
      end record;

      type Cell_Access           is access all Cell_Type;
      type Cell_Class_Access     is access all Cell_Type'class;

      type Configured_Update_Parameter_Type
                                 is new Generic_Cell_Package.
                                    Update_Parameter_Type with record
--       Label                   : Ada_Lib.Strings.Unlimited.String_Type;
         Preset_ID               : Camera.Preset_ID_Type;
      end record;

      procedure Column_Package_Update (
         Cell                 : in     Cell_Class_Access;
         Coordinate           : in     Configuration.Camera.Column_Type
      ) with Pre =>GNOGA_Ada_Lib.Has_Connection_Data;

      overriding
      procedure Create_Cell (
         Cell                    : in out Cell_Type;
         Form                    : in     Gnoga.Gui.Element.Form.
                                          Pointer_To_Botton_Class;
         Row                        : in out Gnoga.Gui.Element.Table.
                                             Table_Row_Type'class;
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre => GNOGA_Ada_Lib.Has_Connection_Data;

      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         From                    : in     String := ADA_LIB.Trace.Here);

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

      type Preset_Column_Type is new
                                    Generic_Cell_Package.
                                       Generic_Column_Type with record
         Cell                    : Cell_Class_Access := Null;
      end record;

      type Preset_Column_Access is access Preset_Column_Type;

      overriding
      function Get_Cell (
         Column                  : in out Preset_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access;

      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Preset_Column_Index_Type;
         Table_Row               : in     Preset_Row_Index_Type);

      overriding
      procedure Create_Column (
         Column                     : in out Preset_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Configuration.Camera.Configuration_ID_Type;
         Row_Index                  : in     Configuration.Camera.Configuration_ID_Type;
         Column_Index               : in     Preset_Column_Index_Type;
         ID                         : in     String);

      type Widget_Type           is new Generic_Cell_Package.Widget_Type
                                    with null record;

      overriding
      function Create_Column (
         Widget                  : in out Widget_Type;
         Row                     : in     Configuration.Camera.Configuration_ID_Type;
         Column                  : in     Preset_Column_Index_Type
      ) return Boolean;

      overriding
      function Create_Row (
         Widget                  : in out Widget_Type;
         Row                     : in     Configuration.Camera.Configuration_ID_Type
      ) return Boolean;

      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

   end Preset_Package;

   subtype Column_Labels_Type    is Generic_Cell_Package.
                                    Column_Labels_Type;

   subtype Column_Labels_Access  is Generic_Cell_Package.
                                    Column_Labels_Access;

   function Create_Labels (
      Number_Columns             : in        Preset_Column_Index_Type
   ) return Column_Labels_Access;

   package Configured_Package is new Widgets.    -- row for each preset
         Generic_Table.Generic_Table_Package (        -- 1st column description
      Allocate_Column        => Preset_Package.Allocate_Column,
      Column_Header        => True,
      Column_Index_Type    => Preset_Column_Index_Type,
      Create_Column_Labels => Create_Labels,
      Create_Form          => True,
--    Form_Field           => False,
--    Formal_Column_Type   => Preset_Package.Preset_Column_Type,
      Generic_Cell_Package => Generic_Cell_Package,
      Generic_Widget_Type  => Preset_Package.Widget_Type,
      Header_Type          => Gnoga.Gui.Element.Table.Table_Heading_Type,
      On_Submit            => Preset_Package.On_Submit'access,
      Row_Header           => True,
      Row_Index_Type       => Configuration.Camera.Configuration_ID_Type);

   type Configured_Card_Type
                           is new Configured_Package.Widget_Type with
                              null record;
   type Configured_Card_Access
                           is access all Configured_Card_Type;
   type Configured_Card_Class_Access
                           is access all Configured_Card_Type'class;
   procedure Create (
      Configured_Card            : in out Configured_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.View.View_Base_Type'Class
   ) with Pre => GNOGA_Ada_Lib.Has_Connection_Data;

   overriding
   function Get_Form (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Botton_Class;

   overriding
   function Get_Accept_Button (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class;

   overriding
   function Get_Cancel_Button (
      Configured_Card            : in out Configured_Card_Type
   ) return Gnoga.Gui.Element.Form.Pointer_To_Submit_Button_Class;

-- procedure Update_Row_Fields (
--    Configured_Card            : in     Configured_Card_Type;
--    Row_Index                  : in     Row_Index_Type;
--    Configuration_ID           : in     Configuration.Camera.Configuration_ID_Type;
--    Preset_ID                  : in     Camera.Preset_ID_Type;
--    Label                      : in     String;
--    Control_Column             : in     Configuration.Camera.Column_Type;
--    Control_Row                : in     Configuration.Camera.Row_Type);

   overriding
   procedure Verify_Widget (
      Widget                     : in     Configured_Card_Type;
      Verify_Parameter           : in     Configured_Package.
                                             Verify_Parameter_Class_Access);

end Widgets.Configured;



