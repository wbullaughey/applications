with Ada_Lib.Strings.Unlimited;
with ADA_LIB.Trace;
with Camera;
with GNOGA_Ada_Lib;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;
with Widgets.Control;
with Widgets.Generic_Table;

------------------------------------------------------------------------------
-- Configured Card has 2 grids layed out horizontally
-- The left grid has one row per configuration.
--    Each column is a cell type based on the column type
--       1:Row_Header,   row header
--       2:Label_Field,  label
--       3:Preset_Field, preset
--       4:Column_Field, column the preset is in the control grid
--                       row the preset is in the control grid

--       5:Row_Field,    image path for the preset
--       6:Image_Field,
--       7:control
-- The right grid is the Widgets.Control grid
------------------------------------------------------------------------------

package Widgets.Configured is

   use type Configuration.Camera.Configuration_ID_Type;

   Failed                        : exception;

   Debug                         : aliased Boolean := False;
   Widget_Name                   : constant String := "Configured_Card";

   type Cell_Type                is new Gnoga.Gui.Element.Common.DIV_Type
                                    with null record;

   type Preset_Column_Index_Type is (  -- need to be on order columns created
      Row_Header,    -- button for editing configuration id
      Label_Field,   -- display label for presets
      Preset_Field,  -- field for editing preset number
      Column_Field,  -- select which column a preset image should be in
                     -- the preset grid
      Row_Field,     -- select which row a preset image should be in
                     -- the preset grid
      Image_Field,   -- cell with path for image file
      Control_Grid_Field);
                     -- only 1st row has the control grid
   subtype Preset_Row_Index_Type
                              is Configuration.Camera.Configuration_ID_Type;

   package Generic_Cell_Package
      is new Widgets.Generic_Table.Cell_Package (
         Column_Index_Type => Preset_Column_Index_Type,
         Row_Index_Type    => Configuration.Camera.Configuration_ID_Type);

   -- package for web page with control grid
-- package Control_Grid_Package is new Widgets.Generic_Table.Cell_Package (
--                               Preset_Column_Index_Type,
--                               Configuration.Camera.Configuration_ID_Type);

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

      type Cell_Type is abstract new Generic_Cell_Package.Cell_Type with record
         Configuration_ID        : Configuration.Camera.Configuration_ID_Type :=
                                    Configuration.Camera.No_Configuration;
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
                                             GNOGA_Column_Type'class;
         Table_Column            : in     Preset_Column_Index_Type;
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class) is abstract;
      -- preset table cells

      type Column_Cell_Type is new Cell_Type with record
         Column_Coordinate : Gnoga.Gui.Element.Form.Number_Type;
         Column_Number     : Configuration.Camera.Column_Type :=
                              Configuration.Camera.Column_Not_Set;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Column_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Column_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);
      -- used in only 1st row of preset table.
      type Control_Grid_Cell_Type is new Cell_Type with record
         Control_Table     : Widgets.Control.Control_Card_Type;
         Table_Row         : Configuration.Camera.Configuration_ID_Type;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Control_Grid_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Control_Grid_Cell_Type ;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Image_Cell_Type is new Cell_Type with record
         Image_Div         : Image_Div_Type;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Image_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Image_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Label_Cell_Type is new Cell_Type with record
         Label             : Gnoga.Gui.Element.Form.Text_Type;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Label_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := Ada_Lib.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Label_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Preset_Cell_Type is new Cell_Type with record
         Preset_ID         : Camera.Preset_ID_Type;
         Preset_ID_Field   : Gnoga.Gui.Element.Form.Number_Type;
         Preset_Set        : Boolean := False;
      end record;

      overriding
      procedure Dump (
         Cell                    : in     Preset_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Update_Cell (
         Cell              : in out Preset_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Row_Cell_Type is new Cell_Type with record
         Row_Coordinate    : Gnoga.Gui.Element.Form.Number_Type;
         Row_Number        : Configuration.Camera.Row_Type :=
                              Configuration.Camera.Row_Not_Set;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Row_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Row_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Row_Header_Cell_Type is new Cell_Type with record
         Button            : Gnoga.Gui.Element.Common.Button_Type;
      end record;

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
         Table_Row               : in     Configuration.Camera.
                                             Configuration_ID_Type
      ) with Pre  => GNOGA_Ada_Lib.Has_Connection_Data,
             Post => Cell.Configuration_ID /=
                        Configuration.Camera.No_Configuration;

      overriding
      procedure Dump (
         Cell                    : in     Row_Header_Cell_Type;
         Enable                  : in     Boolean;
         Caller                  : in     String;
         From                    : in     String := ADA_LIB.Trace.Here);

      overriding
      procedure Update_Cell (
         Cell              : in out Row_Header_Cell_Type;
         Update_Parameter  : in     Generic_Cell_Package.
                                       Update_Parameter_Type'class);

      type Configured_Update_Parameter_Type
                                 is new Generic_Cell_Package.
                                    Update_Parameter_Type with record
   --       Label                   : Ada_Lib.Strings.Unlimited.String_Type;
         Preset_ID               : Camera.Preset_ID_Type;
      end record;

      type Preset_Column_Type is new
                                    Generic_Cell_Package.
                                       GNOGA_Column_Type with null record;
--       Cell                    : Cell_Class_Access := Null;
--    end record;

      type Preset_Column_Access is access Preset_Column_Type;

      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             GNOGA_Column_Class_Access;
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
      Allocate_Column      => Preset_Package.Allocate_Column,
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



