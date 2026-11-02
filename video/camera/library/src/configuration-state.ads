with Ada_Lib.Configuration;
with ADA_LIB.Strings.Unlimited;
with Camera.Commands;
with Configuration.Configuration;
with Video.Lib;

package Configuration.State is

   Failed               : exception;

   type Address_Key_Type      is array (Video.Lib.Location_Type) of
                                 ADA_LIB.Strings.String_Access;

   type Images_Type              is array (Row_Type range <>,
                                    Column_Type range <>) of
                                       ADA_LIB.Strings.Unlimited.String_Type;
   type Images_Access            is access Images_Type;

   type State_Type               is new Abstract_Configuration_Type
                                    with private;

   type State_Access             is access all State_Type;
   type State_Class_Access       is access all State_Type'class;
   type State_Constant_Access    is access constant State_Type;
   type State_Constant_Class_Access
                                 is access constant State_Type'class;
   type States_Type              is array (Video.Lib.Camera_ID_Type range <>) of
                                    State_Class_Access;
   type States_Access            is access all States_Type;

   function Check_Column (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type;
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => Has_Camera_State (Camera_ID);

-- function Check_Image (
--    Column                     : in     Column_Type;
--    Row                        : in     Row_Type
-- ) return Boolean;

   function Check_Row (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type;
      Row                        : in     Row_Type
   ) return Boolean
   with Pre => Has_Camera_State (Camera_ID);

   procedure Clear_Global_Camera_State (
      State                      : in out State_Type
   ) with Pre  => State.Has_Camera_ID,
          Post => not State.Has_Camera_ID;

   procedure Copy (
      Destination                : in out State_Type;
      Source                     : in     State_Type);

   procedure Dump (
      State                      : in     State_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here);

   procedure Free (
      State                      : in out State_Type);

   function Get_Brand (
      State                      : in     State_Type
   ) return Standard.Camera.Brand_Type;

   function Get_Camera (
      State                      : in     State_Type
   ) return Standard.Camera.Commands.Camera_Class_Access
   with Pre => State.Has_Camera;

   function Get_Camera_ID (
      State                      : in     State_Type
   ) return Standard.Video.Lib.Camera_ID_Type
   with Pre => State.Has_Camera_ID;

   function Get_Camera_Name (
      State                      : in     State_Type
   ) return String
   with Pre => State.Has_Camera_ID;

   function Get_CSS_Path (
      State                      : in     State_Type
   ) return String;

   function Get_Default_Pan (
      State                      : in     State_Type
   ) return Pan_Type;

-- function Get_Default_Pan is new Get_Default_Value (
--    Result_Type => Pan_Type,
--    Accessor    => Get_Default_Pan);

   function Get_Default_Pan_Speed (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Speed_Type
   with Pre => Has_Camera_State (Camera_ID);


   function Get_Default_Tilt (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Tilt_Type
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Default_Tilt_Speed (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Speed_Type
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Default_Zoom (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Zoom_Type
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Default_Zoom_Speed (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Speed_Type
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Modifiable_Global_State (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return State_Access;

-- overriding
   function Get_Number_Columns (
      State                      : in     State_Type
   ) return Column_Type;

-- overriding
   function Get_Number_Configurations (
      State                      : in     State_Type
   ) return Configuration.Configuration_ID_Type;

-- overriding
   function Get_Number_Presets (
      State                      : in     State_Type
   ) return Natural;

-- overriding
   function Get_Number_Rows (
      State                      : in     State_Type
   ) return Row_Type;

-- function Root_State_Type (
--    Camera                     : in     State_Type
-- ) return Ada_Lib.Socket_IO.Address_Constant_Access
-- with Pre => Camera.Has_Video_Address;

   function Get_Video_Address (
      Camera                      : in     State_Type
   ) return Ada_Lib.Socket_IO.Address_Constant_Access;

   function Get_Video_Address_URL (
      Camera                  : in        State_Type
   ) return String
   with Pre => Camera.Has_Video_Address and then
               Camera.Is_URL_Video_Address;

   function Get_Video_Port (
      Camera                  : in        State_Type
   ) return Video.Lib.Port_Type
   with Pre => Camera.Has_Video_Port;

   function Has_Camera (
      State                      : in     State_Type
   ) return Boolean;

   function Has_Camera_ID (
      State                      : in     State_Type
   ) return Boolean;

   function Has_Image (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => State.Is_Loaded;
--             Check_Image (Column, Row);

   function Has_Video_Address (
      Camera                      : in     State_Type
   ) return Boolean;

   function Has_Video_Port (
      Camera                      : in     State_Type
   ) return Boolean;

   function Image_Name (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type;
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return String
   with Pre => Check_Column (Camera_ID, Column) and then
               Check_Row (Camera_ID, Row);

   function Image_Path (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type;
      Add_Prefix                 : in     Boolean := False
   ) return String
   with Pre => State.Is_Loaded;
--             Check_Image (
--                Column   => Column,
--                Row      => Row);

   function Is_URL_Video_Address (
      Camera                      : in     State_Type
   ) return Boolean;

-- procedure Set_State (
--    State                      : in     State_Access;
--    From                       : in     String := Ada_Lib.Trace.Here
-- ) with Pre  => not Has_Camera_State (Camera_ID),
--        Post => Has_Camera_State (Camera_ID);

-- function Get_Camera (
--    Camera_ID                  : in     Video.Lib.Camera_ID_Type
-- ) return Standard.Camera.Commands.Camera_Class_Access
-- with Pre => Has_Camera_State (Camera_ID);

   function Get_Camera_State (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return State_Access
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Constant_Camera_State (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return State_Constant_Access
   with Pre => Has_Camera_State (Camera_ID);

   function Get_Current_Camera_ID
   return Video.Lib.Camera_ID_Type;

   function Has_Camera_State (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type
   ) return Boolean;

-- overriding
   procedure Unload (
      State                      : in out State_Type);

   type Cameras_Type             is array (Video.Lib.Camera_ID_Type range <>)
                                    of aliased State_Type ;

   type Cameras_Access           is access Cameras_Type;

  procedure Load (
     Camera                   : in out State_Type;
     Location                 : in     Video.Lib.Location_Type;
     Path                     : in     String;
     Video_ID                 : in     Video.Video_ID_Type);

-- overriding
   procedure Load (
      State       : in out State_Type;
      Config      : in out Ada_Lib.Configuration.Configuration_Type;
      Location    : in     Video.Lib.Location_Type;
      Video_ID    : in     Video.Video_ID_Type);

   procedure Load_Camera (
      Camera_ID                  : in     Video.Lib.Camera_ID_Type;
      Path                       : in     String);

   Default_State                 : constant String := "state.cfg";


   Adjust_Card_Style    : constant String := "Adjust_Card";
   Blank_Preset_Root    : constant String := "no_image.png";
   Blank_Preset         : constant String := "img/" & Blank_Preset_Root;
   Center_Box_Style     : constant String := "Center_Box_Style";
   Column_Not_Set       : constant := Column_Type'last;
   Configuration_Not_Set: constant := Configuration.Configuration_ID_Type'last;
   Configured_Card_Style: constant String := "Configured_Card";
   Control_Card_Style   : constant String := "Control_Card";
   Control_Cell_Style   : constant String := "Control_Cell";
   Control_Text_Style   : constant String := "Control_Text";
   Coordinate_Style     : constant String := "Coordinate_Style";
   Control_Image_Style  : constant String := "Control_Image";
   Horizontal_Slider_Style
                        : constant String := "Horizontal_Slider_Style";

   Preset_Style         : constant String := "Preset_Style";
   Row_Not_Set          : constant := Row_Type'last;
   Vertical_Slider_Blank_Style: constant String := "Vertical_Slider_Blank_Style";
   Vertical_Slider_Solid_Style: constant String := "Vertical_Slider_Solid_Style";

private

   type State_Type            is new Abstract_Configuration_Type
                                 with record
      Brand                   : Ada_Lib.Strings.Unlimited.String_Type;
      Camera                  : Standard.Camera.Commands.Camera_Class_Access :=
                                 Null;
      Camera_ID               : Standard.Video.Lib.Camera_ID_Type;
      Camera_Name             : Ada_Lib.Strings.Unlimited.String_Type;
      CSS_Path                : ADA_LIB.Strings.Unlimited.String_Type;
      Default_Pan             : Pan_Type;
      Default_Pan_Speed       : Speed_Type;
      Default_Tilt            : Tilt_Type;
      Default_Tilt_Speed      : Speed_Type;
      Default_Zoom            : Zoom_Type;
      Default_Zoom_Speed      : Speed_Type;
      Images                  : Images_Access := Null;
                                 -- pointer two dimensional array of image paths
                                 -- 1st dimension is row, second is column
      Number_Columns          : Column_Type;
      Number_Configurations   : Configuration.Configuration_ID_Type;
      Number_Rows             : Row_Type;
      Simulate                : Boolean := False;
      Video_Address           : Video.Lib.Address_Constant_Access := Null;
      Video_Port              : Video.Lib.Port_Type :=
                                 Video.Lib.Port_Type'last;
   end record;

end Configuration.State;
