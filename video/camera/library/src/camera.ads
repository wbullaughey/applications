with Ada.Containers;
with Ada_Lib.GNOGA;
with Ada_Lib.Options.Nested;
with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.Trace;
with Configuration.State;
with Gnoga_Ada_Lib;
with Hex_IO;
with Interfaces;
with Video.Lib;

package Camera is

   Failed                        : exception;

   use type Ada_Lib.Socket_IO.Address_Kind_Type;
   use type Video.Lib.Relative_Type;
   use type Video.Lib.Location_Type;

   subtype Address_Type          is Video.Lib.Address_Type;
   subtype Address_Constant_Access
                                 is Video.Lib.Address_Constant_Access;
   subtype Address_Kind_Type     is Video.Lib.Address_Kind_Type;
   type Brand_Type               is (ALPTOP_Camera, PTZ_Optics_Camera, No_Camera);
   subtype Buffer_Type           is Video.Lib.Buffer_Type;
   type Abstract_Window_Connection_Type
                                 is abstract new Ada_Lib.GNOGA.Connection_Data_Type
                                    with null record; -- connection data for windows

   type Camera_ID_Type           is tagged private;

   procedure Dump (
      Camera_ID                  : in        Camera_ID_Type);

   function Image (
      Camera_ID                  : in        Camera_ID_Type
   ) return String;

   function Is_Set (
      Camera_ID                  : in        Camera_ID_Type
   ) return Boolean;

   function Make_Camera_ID (
      Address                    : in        Address_Type
   ) return Camera_ID_Type
   with Pre => Address.Address_Kind /= Ada_Lib.Socket_IO.Not_Set;

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
      Memory_Set,
      Memory_Reset,
      Power,
      Power_Inquire,
      Recall_Speed,
      Zoom_Direct,
      Zoom_Inquire,
      Zoom_Stop,
      Zoom_Tele_Standard,
      Zoom_Tele_Variable,
      Zoom_Wide_Standard,
      Zoom_Wide_Variable
   );
   subtype Data_Type             is Video.Lib.Data_Type;
   subtype Index_Type            is Video.Lib.Index_Type;
   subtype Maximum_Response_Type is Video.Lib.Maximum_Response_Type;

   type Mouse_Click_Action_Type  is (Any_Scroll, Horizontal_Scroll,
                                       No_Action, No_Change, Vertical_Scroll);

   type Options_Mode_Type        is (Add, Fixed, Variable);
   subtype Value_Type            is Video.Lib.Value_Type;

   type Command_Option_Type (
      Mode                       : Options_Mode_Type := Fixed) is record
      Start                      : Index_Type;

      case Mode is

         when Add | Fixed =>
            Data                 : Data_Type;

         when Variable =>
            Value                : Value_Type;
            Width                : Index_Type;

      end case;
   end record;

   type Command_Options_Type     is array (Index_Type range  <>) of Command_Option_Type;

   subtype Port_Type             is Video.Lib.Port_Type;

   type Camera_Options_Type is limited new Video.Lib.Options_Type with record
      Brand          : Brand_Type := PTZ_Optics_Camera;
      Camera_Address : Address_Constant_Access := Null;
      Camera_ID      : Camera_ID_Type;
--    Location       : Configuration.State.Location_Type :=
--                      Video.Lib.No_Location;
      Port_Number    : Port_Type; -- := Standard.Camera.Commands.PTZ_Optics.Port;
   end record;

   type Camera_Options_Access           is access all Camera_Options_Type;
   type Camera_Options_Class_Access     is access all Camera_Options_Type'class;
   type Camera_Options_Constant_Class_Access
                                 is access constant Camera_Options_Type'class;

   function Has_Location (
      Location       : in     Configuration.State.Location_Type
   ) return Boolean;

   overriding
   function Initialize (
      Options               : in out Camera_Options_Type;
      From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre    => Options.Verify_Preinitialize,
        post   => Options.Verify_Initialized;

   overriding
   function Process_Option (  -- process one option
      Options  : in out Camera_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Was_Initialized;

   overriding
   procedure Program_Help (
      Options                    : in     Camera_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   overriding
   procedure Trace_Parse (
      Options                    : in out Camera_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class);

   procedure Process_Command (
      Connection_Data            : in out Abstract_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Timeout_Time               : in     Duration := 0.0) is abstract;
                                          -- when 0 use command default

   procedure Process_Command (
      Connection_Data            : in out Abstract_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0) is abstract;
                                          -- when 0 use command default

   subtype Absolute_Type         is Interfaces.Integer_16;

   subtype Maximum_Command_Type  is Video.Lib.Maximum_Command_Type;
   subtype Response_Type         is Video.Lib.Response_Type;
   subtype Preset_ID_Type        is Video.Lib.Preset_ID_Type;
   subtype Preset_Range_Type     is Video.Lib.Preset_Range_Type;
   subtype Property_Type         is Data_Type range 0 .. 255; -- 2**8;
   subtype Relative_Type         is Video.Lib.Relative_Type range -2**15 .. 2**15;
   type Zoom_Type                is new Video.Lib.Value_Type range 0 .. 16#FFFF#;

   function Camera_ID_Equal (
      Left, Right                : in     Camera_ID_Type
   ) return Boolean;

   function Camera_ID_Hash (
      Key                        : in     Camera_ID_Type
   ) return Ada.Containers.Hash_Type;

   function Camera_Hash (
      Address                    : in     Address_Type
   ) return Ada.Containers.Hash_Type;

   procedure Dump (
      Description                : in     String;
      Data                       : in     Buffer_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) renames Ada_Lib.Socket_IO.Stream_IO.Dump;

   function Hex is new Hex_IO.Modular_Hex (Data_Type);

   function Image (
      Value                      : in     Data_Type
   ) return String renames Video.Lib.Image;

   IP                            : Address_Kind_Type := Ada_Lib.Socket_IO.IP;
   NOT_SET                       : Address_Kind_Type := Ada_Lib.Socket_IO.NOT_SET;
   Null_Camera_ID                : constant Camera_ID_Type;
   Null_Options                  : constant Command_Options_Type;
   URL                           : Address_Kind_Type := Ada_Lib.Socket_IO.URL;

private

   type Camera_ID_Type           is tagged record
      Set                        : Boolean := False;
      Value                      : Ada.Containers.Hash_Type;
   end record;

   Null_Camera_ID                : constant Camera_ID_Type := (
      Set   => False,
      Value => 0);
   Null_Options                  : constant Command_Options_Type (1 .. 0) :=
                                    (others => <>);
end Camera;
