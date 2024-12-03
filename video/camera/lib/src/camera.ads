--with Ada_Lib.GNOGA;
with Ada_Lib.Socket_IO; --.Stream_IO;
with ADA_LIB.Trace;
with Interfaces;
with Video.Lib;

package Camera is

   use type Video.Lib.Relative_Type;

   subtype Absolute_Type         is Interfaces.Integer_16;
   subtype Address_Type          is Video.Lib.Address_Type;
   subtype Address_Constant_Access
                                 is Video.Lib.Address_Constant_Access;
   subtype Address_Kind_Type     is Video.Lib.Address_Kind_Type;
   subtype Response_Buffer_Type  is Video.Lib.Response_Buffer_Type;
   type Response_Buffer_Class_Access
                                 is access all Response_Buffer_Type'class;
   subtype Camera_Preset_Type    is Video.Lib.Camera_Preset_Type; -- range
--                                    Video.Lib.Unset_Preset .. 127;
   subtype Camera_Type           is Video.Lib.Camera_Type;

   type Commands_Type is (
      Auto_Focus,
      Manual_Focus,
      Position_Absolute,
      Position_Down_Left,
      Position_Down_Right,
      Position_Down,
      Position_Left,
      Position_Relative,
      Position_Request,
      Position_Right,
      Position_Stop,
      Position_Up,
      Position_Up_Left,
      Position_Up_Right,
      Memory_Recall,
      Memory_Set,          -- updates a preset to current location
      Memory_Reset,
      Power,
      Power_Request,
      Zoom_Direct,
      Zoom_Full,
      Zoom_Inquire,
      No_Command
   );

   subtype Buffer_Type           is Video.Lib.Buffer_Type;
   subtype Data_Type             is Video.Lib.Data_Type;
   subtype Index_Type            is Video.Lib.Index_Type;
   subtype Maximum_Command_Type  is Video.Lib.Maximum_Command_Type;
   subtype Maximum_Response_Type is Video.Lib.Maximum_Response_Type;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   subtype Relative_Type         is Video.Lib.Relative_Type range -2**15 .. 2**15;
-- subtype Relative_Type         is Video.Lib.Relative_Type range -16#EF# .. 16#EF#;
   subtype Response_Type         is Video.Lib.Response_Type;
   subtype Port_Type             is Video.Lib.Port_Type;
   subtype Status_Type           is Video.Lib.Status_Type;
   Fault                         : Status_Type renames Video.Lib.Fault;
   Not_Set                       : Status_Type renames Video.Lib.Not_Set;
   Success                       : Status_Type renames Video.Lib.Success;
   Timeout                       : Status_Type renames Video.Lib.Timeout;
   subtype Value_Type            is Video.Lib.Value_Type;
   type Option_Type (
      Variable_Width             : Boolean := False) is record
      Start                      : Index_Type;

      case Variable_Width is

         when False =>
            Data                 : Data_Type;

         when True =>
            Value                : Value_Type;
            Width                : Index_Type;

      end case;
   end record;

   Maximum_Options               : constant := 10;
   type Options_Type             is array (Index_Type range  <>) of Option_Type;
   subtype Maximum_Options_Type  is Options_Type (1 .. Maximum_Options);

   type Zoom_Type                is new Video.Lib.Value_Type range 0 .. 16#FFFF#;

   function Convert (
      Absolute                   : in     Absolute_Type
   ) return Value_Type;

   function Convert (
      Relative                   : in     Relative_Type
   ) return Value_Type;

   procedure Dump (
      Response                   : in     Response_Buffer_Type;
      Description                : in     String;
      Length                     : in     Natural := 0;
      From                       : in     String := Ada_Lib.Trace.Here
   ) renames Video.Lib.Dump;

   procedure Dump (
      Description                : in     String := "";
      Buffer                     : in     Buffer_Type;
      Length                     : in     Natural := 0;
      From                       : in     String := Ada_Lib.Trace.Here
   ) renames Video.Lib.Dump;

   function Image (
      Value                      : in     Data_Type
   ) return String renames Video.Lib.Image;

   Debug                         : Boolean := False;
   IP                            : Address_Kind_Type := Ada_Lib.Socket_IO.IP;
   Null_Option                   : constant Option_Type;
   Null_Options                  : constant Options_Type;
   URL                           : Address_Kind_Type := Ada_Lib.Socket_IO.URL;

private

   Null_Option                   : constant Option_Type := Option_Type'(
                                       Data              => 0,
                                       Start             => 0,
                                       Variable_Width    => False);
   Null_Options                   : constant Options_Type (1 .. 0) :=
                                    ( others => Null_Option);
end Camera;
