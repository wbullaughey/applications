--with Ada_Lib.GNOGA;
with Ada_Lib.Socket_IO.Stream_IO;
with ADA_LIB.Trace;
with Interfaces;
with Video.Lib;

package Camera is

   use type Video.Lib.Relative_Type;

   Maximum_Preset                : constant := 254;

   subtype Address_Type          is Video.Lib.Address_Type;
   subtype Address_Constant_Access
                                 is Video.Lib.Address_Constant_Access;
   subtype Address_Kind_Type     is Video.Lib.Address_Kind_Type;
   subtype Buffer_Type           is Video.Lib.Buffer_Type;
   type Camera_Preset_And_Not_Set_Type
                                 is new Video.Lib.Data_Type range
                                    0 .. Video.Lib.Unset_Preset;
   subtype Camera_Preset_Type    is Video.Lib.Data_Type range
                                    0 .. Maximum_Preset;
   subtype Camera_Type           is Video.Lib.Camera_Type;
   subtype Absolute_Type         is Interfaces.Integer_16;

   subtype Data_Type             is Video.Lib.Data_Type;
   subtype Index_Type            is Video.Lib.Index_Type;
   subtype Maximum_Command_Type  is Video.Lib.Maximum_Command_Type;
   subtype Maximum_Response_Type is Video.Lib.Maximum_Response_Type;
   subtype Response_Type         is Video.Lib.Response_Type;
   subtype Port_Type             is Video.Lib.Port_Type;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   subtype Relative_Type         is Video.Lib.Relative_Type range -2**15 .. 2**15;
   subtype Value_Type            is Video.Lib.Value_Type;
   type Zoom_Type                is new Video.Lib.Value_Type range 0 .. 16#FFFF#;

   procedure Dump (
      Description                : in     String;
      Data                       : in     Buffer_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) renames Ada_Lib.Socket_IO.Stream_IO.Dump;

   function Hex (
      Data                       : in     Data_Type;
      Width                      : in   Positive := Data_Type'size / 4
   ) return String renames Ada_Lib.Socket_IO.Hex;

   function Image (
      Value                      : in     Data_Type
   ) return String renames Video.Lib.Image;

   IP                            : Address_Kind_Type := Ada_Lib.Socket_IO.IP;
   NOT_SET                       : Address_Kind_Type := Ada_Lib.Socket_IO.NOT_SET;
   URL                           : Address_Kind_Type := Ada_Lib.Socket_IO.URL;

end Camera;
