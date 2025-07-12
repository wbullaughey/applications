with Ada_Lib.Socket_IO.Client;
with Ada_Lib.Strings;
with Configuration.Camera;
with GNAT.Sockets;

package Camera.Lib.Base is

   Failed                        : exception;
   Timed_Out                     : exception;

   use type GNAT.Sockets.Port_Type;

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
      Zoom_Direct,
      Zoom_Inquire,
      Zoom_Stop,
      Zoom_Tele_Standard,
      Zoom_Tele_Variable,
      Zoom_Wide_Standard,
      Zoom_Wide_Variable
   );

   type Options_Mode_Type        is (Add, Fixed, Variable);
   type Option_Type (
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

   type Options_Type             is array (Index_Type range  <>) of Option_Type;

   type Command_Type             is record
      Length                     : Index_Type;
      Command                    : Maximum_Command_Type;
      Get_Ack                    : Boolean;
      Response_Timeout           : Duration;
      Has_Response               : Boolean;
      Response_Length            : Index_Type;
   end record;

   type Base_Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new General_Camera_Type with private;

   type Base_Camera_Class_Access is access all Base_Camera_Type'class;

   procedure Acked (
      Camera                     : in     Base_Camera_Type;
      Response                   : in     Buffer_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Index_Type) is abstract;

   procedure Dump_Input_Buffer (
      Camera                     : in out Base_Camera_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

-- procedure Cell_Preset (
--    Camera                     : in out Base_Camera_Type;
--    Preset                     : in     Camera_Preset_Type) is abstract;

   overriding
   procedure Close (
      Camera                     : in out Base_Camera_Type);

   procedure Completed (
      Camera                     : in     Base_Camera_Type;
      Buffer                     : in     Buffer_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type) is abstract;

   function Get_Ack_Length (
      Camera                     : in     Base_Camera_Type
   ) return Index_Type is abstract;

   function Get_Default_Preset (
      Camera                     : in     Base_Camera_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   function Get_Maximum_Preset (
      Camera                     : in     Base_Camera_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   procedure Get_Response (
      Camera                     : in out Base_Camera_Type;
      Expect_Ack                 : in     Boolean;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration);

   function Get_Timeout (
      Camera                     : in     Base_Camera_Type;
      Command                    : in     Commands_Type
   ) return Duration is abstract;

   overriding
   procedure Host_Open (
      Camera                     :    out Base_Camera_Type;
      Host_Address               : in     String;
      Port                       : in     GNAT.Sockets.Port_Type);

   overriding
   procedure IP_Open (
      Camera                     :    out Base_Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     GNAT.Sockets.Port_Type
   ) with Pre => Port /= 0;

   overriding
   procedure Open (
      Camera                     :    out Base_Camera_Type;
      Address                    : in     Ada_Lib.Socket_IO.Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type);

   procedure Send_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Send_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Timeout_Time               : in     Duration := 0.0);
                                          -- when 0 use command default

   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0);
                                          -- when 0 use command default

   procedure Process_Response (
      Camera                     : in     Base_Camera_Type;
      Response                   : in     Buffer_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Index_Type) is abstract;

   overriding
   procedure URL_Open (
      Camera                     :    out Base_Camera_Type;
      URL                        : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type);

   procedure Write (
      Camera                     :    out Base_Camera_Type;
      Data                       : in     Data_Type);

   procedure Write (
      Camera                     :    out Base_Camera_Type;
      Data                       : in     Buffer_Type);

   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Options                    : in     Options_Type);

   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Command                    : in     Buffer_Type;
      Options                    : in     Options_Type);

   Debug                         : Boolean := False;
   List_Commands                 : Boolean := False;
   Null_Option                   : constant Options_Type;
   Power_On_Preset               : constant := 0;

private

-- Description                   : aliased constant String := "camera";

   type Base_Camera_Type (
      Description                : Ada_Lib.Strings.String_Constant_Access
   ) is abstract new General_Camera_Type with record
      Socket                     : Ada_Lib.Socket_IO.Client.Client_Socket_Type (
                                     Description);
--    Stream                     : Ada_Lib.Socket_IO.Stream_IO.Stream_Type;
   end record;

   Null_Option                   : constant Options_Type (1 .. 0) :=
                                    ( others => (
                                       Data              => 0,
                                       Start             => 0,
                                       Mode              => Fixed));

end Camera.Lib.Base;


