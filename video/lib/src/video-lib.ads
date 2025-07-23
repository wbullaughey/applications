--with Ada.Streams;
--with ADA_LIB.Command_Line_Iterator;
with Ada_Lib.Options;
with Ada_Lib.Trace;
with ADA_LIB.Options.Actual;
with Ada_Lib.Socket_IO; -- .Stream_IO;
with Ada_Lib.Strings.Unlimited;
with GNAT.Sockets;
with Hex_IO;

package Video.Lib is

   Failed                        : exception;

   subtype Address_Kind_Type     is Ada_Lib.Socket_IO.Address_Kind_Type;

   IP                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.IP;
   NOT_SET                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.NOT_SET;
   URL                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.URL;
   subtype Address_Type          is  Ada_Lib.Socket_IO.Address_Type;
   subtype Address_Constant_Access
                                 is  Ada_Lib.Socket_IO.Address_Constant_Access;
   subtype Buffer_Type           is Ada_Lib.Socket_IO.Buffer_Type;
   type Camera_Type              is abstract tagged limited null record;
   type Camera_Class_Access      is access all Camera_Type'class;
   subtype Data_Type             is Ada_Lib.Socket_IO.Data_Type;
   type Location_Type            is (Local, Remote);
   type Preset_ID_Type           is tagged private;
   subtype Preset_Range_Type     is Data_Type;

   function Constructor (
      ID                         : in     Preset_Range_Type
   ) return Preset_ID_Type;

   function ID (
      Preset_ID                  : in     Preset_ID_Type
   ) return Preset_Range_Type;

   function Is_Set (
      Preset_ID                  : in     Preset_ID_Type
   ) return Boolean;

   function Null_ID
   return Preset_ID_Type;

   procedure Set (
      Preset_ID                  : in out Preset_ID_Type;
      ID                         : in     Preset_Range_Type);

   procedure Close (

   type Port_Type                is new Ada_Lib.Socket_IO.Port_Type;

   procedure Close (
      Camera                     : in out Camera_Type) is abstract;

   procedure Host_Open (
      Camera                     :    out Camera_Type;
      Host_Address               : in     String;
      Port                       : in     Port_Type) is abstract;

   procedure IP_Open (
      Camera                     :    out Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     Port_Type) is abstract;

   procedure Open (
      Camera                     :     out Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type) is abstract;

   function Parse_Image_Value (
      Value                      : in     String;
      Preset                     :    out Preset_ID_Type
   ) return String;

   function Get_Preset_Not_Set (
      Camera                     : in     Camera_Type
   ) return Preset_ID_Type'class is abstract;

   procedure URL_Open (
      Camera                     :    out Camera_Type;
      URL                        : in     String;
      Port                       : in     Port_Type) is abstract;

   function Image (
      Value                      : in     Data_Type
   ) return String;

   subtype Index_Type            is Ada_Lib.Socket_IO.Index_Type;
   subtype Response_Type         is Ada_Lib.Socket_IO.Buffer_Type;

   subtype Maximum_Command_Type is Buffer_Type (1 .. 20);
   subtype Maximum_Response_Type
                                 is Buffer_Type (1 .. 30);

   type Options_Type             is limited new Ada_Lib.Options.Actual.
                                    Nested_Options_Type with record
      Address_Kind               : Address_Kind_Type;
      If_Emulation               : Boolean := False;
      Location                   : Location_Type := Remote;
      Camera_Address             : Ada_Lib.Socket_IO.Address_Access := Null;
      Port_Number                : Port_Type;
   end record;

   type Options_Access           is access all Options_Type;
   type Options_Class_Access     is access all Options_Type'class;
   type Options_Constant_Class_Access
                                 is access constant Options_Type'class;

   function Address_Kind (
     Options                     : in     Options_Type
   ) return Address_Kind_Type;

   overriding
   function Initialize (
     Options                     : in out Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
     Options                     : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;
-- with Pre => not Ada_Lib.Options.Have_Options;

   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class);

   type Relative_Type            is new Integer;
   type Value_Type               is mod 2**32;

   procedure Dump (
      Description                : in     String;
      Buffer                     : in     Buffer_Type;
      Length                     : in     Natural;
      From                       : in     String := Ada_Lib.Trace.Here);

   function Hex is new Hex_IO.Modular_Hex (Data_Type);
   function Hex is new Hex_IO.Modular_Hex (Value_Type);

   Debug                         : Boolean := False;
-- Global_Video_Lib_Options      : Options_Constant_Class_Access := Null;

private

   type Preset_ID_Type           is tagged record
      Set                        : Boolean := False;
      Id                         : Ada_Lib.Socket_IO.Data_Type :=
                                    Ada_Lib.Socket_IO.Data_Type'last;
   end record;

   Address                       : Ada_Lib.Strings.Unlimited.String_Type;
   Port                          : Port_Type;

   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Video.Lib;


