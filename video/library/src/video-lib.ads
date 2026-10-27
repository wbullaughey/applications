with Ada_Lib.Options.Program;
with Ada_Lib.Options.Verification;
with Ada_Lib.Trace;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
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

   -- simple types
   subtype Address_Type          is  Ada_Lib.Socket_IO.Address_Type;
   subtype Address_Constant_Access
                                 is  Ada_Lib.Socket_IO.Address_Constant_Access;
   subtype Buffer_Type           is Ada_Lib.Socket_IO.Buffer_Type;
   Max_Cameras                   : constant := 25;
   Camera_ID_Not_Set             : constant := Max_Cameras + 1;
   type Camera_ID_Type           is new Positive range 1 .. Camera_ID_Not_Set;
   subtype Cameras_Index_Type    is Camera_ID_Type range 1 .. Max_Cameras;
   subtype Data_Type             is Ada_Lib.Socket_IO.Data_Type;
   subtype Index_Type            is Ada_Lib.Socket_IO.Index_Type;
   type Location_Type            is (Local, Remote, No_Location);
   subtype Port_Type             is Ada_Lib.Socket_IO.Port_Type;
   subtype Maximum_Command_Type is Buffer_Type (1 .. 20);
   subtype Maximum_Response_Type
                                 is Buffer_Type (1 .. 30);
   subtype Response_Type         is Ada_Lib.Socket_IO.Buffer_Type;
   -- class types
   type Camera_Type              is abstract tagged limited null record;
   type Camera_Class_Access      is access all Camera_Type'class;

   procedure Close (
      Camera                     : in out Camera_Type) is abstract;

   procedure Host_Open (
      Camera                     :    out Camera_Type;
      Host_Address               : in     String;
      Port                       : in     Port_Type) is abstract;

   function Image (
      Camera                     : in     Camera_Type
   ) return String is abstract;

   procedure Initialize_Standard_Preset_IDs (
      Camera                     : in     Camera_Type) is abstract;

   procedure IP_Open (
      Camera                     :    out Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     Port_Type) is abstract;

   procedure Open (
      Camera                     :     out Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type) is abstract;

   procedure URL_Open (
      Camera                     :    out Camera_Type;
      URL                        : in     String;
      Port                       : in     Port_Type) is abstract;

   type Relative_Type   is new Integer;
   type Value_Type      is mod 2**32;

   type Video_Lib_Nested_Options_Type (
      Multi_Test        : Boolean
   ) is abstract limited new Ada_Lib.Options.Program.
                           Nested_Program_Options_Type with record
-- ) is abstract limited new Ada_Lib.Options.Verification.
--                         Verification_Nested_Options_Type with record
      Address_Kind      : Address_Kind_Type;
      Directory         : ADA_LIB.Strings.Unlimited.String_Type;
                           -- set by runstring option 'c'
      If_Emulation      : Boolean := False;
      Location          : Location_Type := Local;
      Simulate          : Boolean := False;
   end record;

   type Video_Lib_Nested_Options_Access
                        is access all Video_Lib_Nested_Options_Type;
   type Video_Lib_Nested_Options_Class_Access
                        is access all Video_Lib_Nested_Options_Type'class;
   type Video_Lib_Nested_Options_Constant_Class_Access
                        is access constant Video_Lib_Nested_Options_Type'class;

-- function Address_Kind (
--   Options                     : in     Video_Lib_Nested_Options_Type
-- ) return Address_Kind_Type;

   function Get_Video_Lib_Read_Only_Nested_Options (
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video_Lib_Nested_Options_Constant_Class_Access
   with Pre    => Ada_Lib.Options.Verification.
                     Have_Ada_Lib_Verification_Options,
        Post   => Get_Video_Lib_Read_Only_Nested_Options'result /= Null;

   overriding
   function Initialize (
     Options                     : in out Video_Lib_Nested_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean;
-- with pre    => not Options.Verify_Step (Ada_Lib.Options.Initialized),
--      post   => Options.Verify_Step (Ada_Lib.Options.Initialized);

-- overriding
-- procedure Post_Process (
--   Options                    : in out Video_Lib_Nested_Options_Type);

   overriding
   function Process_Option (  -- process one option
     Options   : in out Video_Lib_Nested_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Step (Ada_Lib.Options.Initialized);
-- with Pre => not Ada_Lib.Options.Have_Options;

   overriding
   procedure Trace_Parse (
      Options                    : in out Video_Lib_Nested_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class);

   function Camera_Configured
   return Boolean
   with Pre    => Ada_Lib.Options.Verification.Have_Ada_Lib_Verification_Options;

   procedure Dump (
      Description                : in     String;
      Buffer                     : in     Buffer_Type;
      Length                     : in     Natural;
      From                       : in     String := Ada_Lib.Trace.Here);

   function Get_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Camera_Class_Access
   with Pre => Has_Camera (Camera_ID);

   function Has_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean;

   function Hex is new Hex_IO.Modular_Hex (Data_Type);
   function Hex is new Hex_IO.Modular_Hex (Value_Type);

   function Image (
      Value                      : in     Data_Type
   ) return String;

-- function Parse_Image_Value (
--    Value                      : in     String;
--    Preset                     :    out Preset_ID_Type'class
-- ) return String;

   package Video_Options is

      Configuration_Debug        : Boolean := False;
      Configuration_State_Debug  : Boolean := False;
      Library_Debug              : Boolean := False;
      Presets_Debug              : Boolean := False;

   end Video_Options;

private

   Address                       : Ada_Lib.Strings.Unlimited.String_Type;
   Port                          : Port_Type;

   overriding
   procedure Program_Help (
      Options                    : in     Video_Lib_Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Video.Lib;


