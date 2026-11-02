--with Ada.Tags;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options.Verification;
with Ada_Lib.Time;
with Ada_Lib.Trace;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with GNAT.Sockets;
with Hex_IO;
with Interfaces;

package Video.Lib is

   Failed                        : exception;

   subtype Address_Kind_Type     is Ada_Lib.Socket_IO.Address_Kind_Type;

   IP                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.IP;
   NOT_SET                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.NOT_SET;
   URL                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.URL;
   Wait_Until_Finished_Time       : constant := 60.0;

   -- simple types
   subtype Absolute_Type         is Interfaces.Integer_16;
   subtype Address_Type          is  Ada_Lib.Socket_IO.Address_Type;
   subtype Address_Constant_Access
                                 is  Ada_Lib.Socket_IO.Address_Constant_Access;

-- type Brand_Tag_Type           is new Ada.Tags.Tag;
-- No_Brand                      : constant Brand_Tag_Type :=
--                                  Brand_Tag_Type (Ada.Tags.No_Tag);
   Max_Cameras                   : constant := 25;
   Camera_ID_Not_Set             : constant := 0;
   type Camera_ID_Type           is new Video_ID_Type  range 0 .. Max_Cameras;
   No_Camera                     : constant Camera_ID_Type := Camera_ID_Type'first;

   subtype Camera_Index_Type     is Camera_ID_Type range 1 .. Max_Cameras;
   type Location_Type            is (Local, Remote, No_Location);
   subtype Port_Type             is Ada_Lib.Socket_IO.Port_Type;
   subtype Maximum_Command_Type is Buffer_Type (1 .. 20);
   subtype Maximum_Response_Type
                                 is Buffer_Type (1 .. 30);
   type Preset_ID_Type           is new Data_Type;
   Preset_ID_Not_Set             : constant Preset_ID_Type :=
                                    Preset_ID_Type'last;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   type Relative_Type            is new Integer range -2**15 .. 2**15;
-- subtype Response_Type         is Ada_Lib.Socket_IO.Buffer_Type;
   -- class types
   type Camera_Type              is abstract tagged limited  null record;
   type Camera_Access            is access all Camera_Type;
   type Camera_Class_Access      is access all Camera_Type'class;

   procedure Close (
      Camera                     : in out Camera_Type) is abstract;

   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time) is abstract;

   function Get_Brand_Name (
      Camera                     : in     Camera_Type
   ) return String is abstract;

   function Hex is new Hex_IO.Modular_Hex (Preset_ID_Type);

   procedure Host_Open (
      Camera                     :    out Camera_Type;
      Host_Address               : in     String;
      Port                       : in     Port_Type) is abstract;

   function Image (
      Camera                     : in     Camera_Type
   ) return String is abstract;

   procedure IP_Open (
      Camera                     :    out Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     Port_Type) is abstract;

   procedure Open (
      Camera                     :     out Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type) is abstract;

   procedure Relative_Command (
      Camera                     : in out Camera_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is abstract;

   -- sets camera to a preset
   procedure Set_Preset (
      Camera                     : in out Camera_Type;
      Preset_ID                  : in     Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True;
      Speed                      : in     Property_Type := 0) is abstract;
                                               -- 0 => default
   procedure URL_Open (
      Camera                     :    out Camera_Type;
      URL                        : in     String;
      Port                       : in     Port_Type) is abstract;


   type Allocator_Type           is access function
   return Camera_Access;

   type Object_Type           is abstract new Video_Type with Private;
   type Object_Class_Access   is access Object_Type'class;

   procedure Dump (
      Object                  : in     Object_Type;
      What                    : in     String := "";
      From                    : in     String := Ada_Lib.Trace.Here);

   function Get_Video_ID (
      Object  :     Object_Type
   ) return Video_ID_Type;

   procedure Initialize (
      Object      :    out Object_Type;
      Video_ID    : in     Video_ID_Type);

   type Video_Lib_Nested_Options_Type (
      Multi_Test        : Boolean
   ) is abstract limited new Ada_Lib.Options.Unit_Test.
      Ada_Lib_Unit_Test_Nested_Options_Type (Multi_Test) with record
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

   function Get_Video_Lib_Constant_Nested_Options (
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video_Lib_Nested_Options_Constant_Class_Access
   with Pre    => Ada_Lib.Options.Verification.
                     Have_Ada_Lib_Verification_Options,
        Post   => Get_Video_Lib_Constant_Nested_Options'result /= Null;

   overriding
   function Initialize (
     Options                     : in out Video_Lib_Nested_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean;
-- with pre    => not Options.Verify_Step (Ada_Lib.Options.Initialized),
--      post   => Options.Verify_Step (Ada_Lib.Options.Initialized);

   overriding
   function Process_Option (  -- process one option
     Options   : in out Video_Lib_Nested_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Step (Ada_Lib.Options.Initialized);

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

   procedure Dump is new Hex_IO.Integer_Dump (
      Index_Type  => Index_Type,
      Value_Type  => Data_Type,
      Buffer_Type => Buffer_Type);

   function Get_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Camera_Class_Access
   with Pre => Has_Camera (Camera_ID);

   function Has_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean;

-- function Hex is new Hex_IO.Hex (Data_Type);
-- function Hex is new Hex_IO.Hex (Value_Type);

   function Image (
      Value                      : in     Data_Type
   ) return String;

   function Is_Valid (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean;

-- procedure Register_Brand (
--    Name                       : in     String;
--    Brand                      : in     Brand_Tag_Type);

   procedure Set_Camera (
      Camera_ID      : in     Camera_Index_Type;
      Camera         : in     Camera_Class_Access
   ) with Pre => Camera /= Null;

   procedure Register_Brand (
      Brand                      : in     String;
      Allocator                  : in     Allocator_Type);

   package Video_Options is

      Camera_Setup_Debug         : Boolean := False;
      Configuration_Debug        : Boolean := False;
      Configuration_State_Debug  : Boolean := False;
      Generic_Debug              : Boolean := False;
      Library_Debug              : Boolean := False;
      Object_Debug               : Boolean := False;
      Presets_Debug              : Boolean := False;
      Setup_Debug                : Boolean := False;
      State_Debug                : Boolean := False;
      Video_Debug                : Boolean := False;

   end Video_Options;

private

   Address                       : Ada_Lib.Strings.Unlimited.String_Type;
   Port                          : Port_Type;

-- type Brand_Type is record
--    Camera_Tag                 : Brand_Tag_Type;
--    Name                       : Ada_Lib.Strings.Unlimited.String_Type;
-- end record;

-- type Camera_Type              is abstract tagged limited record
--    Brand                      : Brand_Type;
-- end record;

   type Object_Type    is abstract new Video_Type with record
      Video_ID        : Video_ID_Type;
   end record;

   overriding
   procedure Program_Help (
      Options                    : in     Video_Lib_Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Video.Lib;


