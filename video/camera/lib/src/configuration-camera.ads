--with Ada_Lib.Socket_IO;

package Configuration.Camera is

   Failed                        : exception;

   type Column_Type              is new Positive;

   type Configuration_ID_Type    is new Positive;

   type Preset_ID_Type           is new Natural;

   type Row_Type                 is new Positive;

   Adjust_Card_Style             : constant String := "Adjust_Card";
   Blank_Preset                  : constant String := "img/no_image.png";
   Center_Box_Style              : constant String := "Center_Box_Style";
   Column_Not_Set                : constant := Column_Type'last;
   Configuration_Not_Set         : constant := Configuration_ID_Type'last;
   Configured_Card_Style         : constant String := "Configured_Card";
   Control_Card_Style            : constant String := "Control_Card";
   Control_Cell_Style            : constant String := "Control_Cell";
   Control_Text_Style            : constant String := "Control_Text";
   Coordinate_Style              : constant String := "Coordinate_Style";
   Control_Image_Style           : constant String := "Control_Image";
   Horizontal_Slider_Style       : constant String := "Horizontal_Slider_Style";
   Positive_Not_Set              : constant := Positive'last;
   Preset_Not_Set                : constant := Preset_ID_Type'last;
   Preset_Style                  : constant String := "Preset_Style";
   Row_Not_Set                   : constant := Row_Type'last;
   Vertical_Slider_Blank_Style   : constant String := "Vertical_Slider_Blank_Style";
   Vertical_Slider_Solid_Style   : constant String := "Vertical_Slider_Solid_Style";

   Debug                         : Boolean := False;

end Configuration.Camera;
