with ADA_LIB.Strings.Unlimited;
--with Camera.Configurations;
with Video.Lib;

package Configuration.Camera is

   Failed               : exception;

   type Address_Key_Type      is array (Video.Lib.Location_Type) of
                                 ADA_LIB.Strings.String_Access;

   type Images_Type              is array (Row_Type range <>,
                                    Column_Type range <>) of
                                       ADA_LIB.Strings.Unlimited.String_Type;
   type Images_Access            is access Images_Type;

-- function Check_Column (
--    Camera_ID                  : in     Video.Lib.Camera_ID_Type;
--    Column                     : in     Column_Type
-- ) return Boolean
-- with Pre => Standard.Camera.Configurations.Has_Configuration (Camera_ID);
--
-- function Check_Image (
--    Column                     : in     Column_Type;
--    Row                        : in     Row_Type
-- ) return Boolean;

-- function Check_Row (
--    Camera_ID                  : in     Video.Lib.Camera_ID_Type;
--    Row                        : in     Row_Type
-- ) return Boolean
-- with Pre => Standard.Camera.Configurations.Has_Configuration (Camera_ID);
--
-- function Get_Default_Speed (
--    Camera_ID                  : in     Video.Lib.Camera_ID_Type
-- ) return Speed_Type
-- with Pre => Standard.Camera.Configurations.Has_Configuration (Camera_ID);
--
-- function Image_Name (
--    Camera_ID                  : in     Video.Lib.Camera_ID_Type;
--    Column                     : in     Column_Type;
--    Row                        : in     Row_Type
-- ) return String
-- with Pre => Check_Column (Camera_ID, Column) and then
--             Check_Row (Camera_ID, Row);

-- type Cameras_Type             is array (Video.Lib.Camera_ID_Type range <>)
--                                  of aliased State_Type ;
--
-- type Cameras_Access           is access Cameras_Type;
--
-- procedure Load_Cameras (
--    Path                       : in     String);

   Default_State                 : constant String := "state.cfg";


   Adjust_Card_Style    : constant String := "Adjust_Card";
   Blank_Preset_Root    : constant String := "no_image.png";
   Blank_Preset         : constant String := "img/" & Blank_Preset_Root;
   Center_Box_Style     : constant String := "Center_Box_Style";
   Column_Not_Set       : constant := Column_Type'last;
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

end Configuration.Camera;
