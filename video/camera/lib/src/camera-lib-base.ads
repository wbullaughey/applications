with Ada_Lib.Event;
with Ada_Lib.GNOGA;
with Ada_Lib.Socket_IO.Client;
with Camera.Commands;
with Configuration.Camera.State;
with GNAT.Sockets;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Plugin.Message_Boxes;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Window;
with Main;
with Widgets.Adjust;
with Widgets.Configured;
with Widgets.Control;

package Camera.Lib.Base is

   Failed                        : exception;
   Timed_Out                     : exception;

   use type GNAT.Sockets.Port_Type;

   type Mouse_Click_Action_Type  is (Any_Scroll, Horizontal_Scroll,
                                       No_Action, No_Change, Vertical_Scroll);

   type Connection_Data_Type     is new Ada_Lib.GNOGA.Connection_Data_Type with
                                    record
      Camera                     : Standard.Camera.Commands.Camera_Class_Access :=
                                    Null;
      Camera_Pan                 : Standard.Camera.Commands.Absolute_Type;
      Camera_Pan_Speed           : Standard.Camera.Commands.Property_Type;
      Camera_Tilt                : Standard.Camera.Commands.Absolute_Type;
      Camera_Tilt_Speed          : Standard.Camera.Commands.Property_Type;
      Camera_Zoom                : Standard.Camera.Commands.Property_Type;
      Main_Data                  : access Main.Main_Data_Type;
      Message_Box_Dialog         : Gnoga.Gui.Plugin.jQueryUI.Widget.
                                    Dialog_Access := Null;
      Message_Box_Result         : Gnoga.Gui.Plugin.Message_Boxes.
                                    Message_Box_Result;
      Mouse_Action               : Mouse_Click_Action_Type := No_Action;
                                    -- application responing to
      State                      : Configuration.Camera.State.State_Type;
      Update_Event               : Ada_Lib.Event.Event_Type (
                                    new String'("update event"));
   end record;

   type Connection_Data_Access   is access all Connection_Data_Type;
   type Connection_Data_Class_Access
                                 is access all Connection_Data_Type'class;

   procedure Close_Message_Box (
      Connection_Data            : in out Connection_Data_Type);

   function Get_Adjust_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Adjust.Adjust_Card_Access;

   function Get_Cards (
      Connection_Data            : in out Connection_Data_Type
   ) return Main.Cards_Access_Type;

   function Get_Configured_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Configured.Configured_Card_Access;

   function Get_Control_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Control.Control_Card_Access;

   function Get_Exit_Button (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class;

   function Get_Main_Window (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class;

   function Get_Tabs (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class;

   procedure Halt;

   procedure Initialize (
      Connection_Data            : in out Connection_Data_Type);

   procedure Message_Box (
      Connection_Data            : in out Connection_Data_Type;
      Title, Text                : in     String;
      Style                      : in     Gnoga.Gui.Plugin.Message_Boxes.
                                             Message_Box_Type :=
                                             Gnoga.Gui.Plugin.Message_Boxes.OK_Box);

   procedure Reset_Update_Event (
      Connection_Data            : in out Connection_Data_Type);

   procedure Trigger_Update_Event (
      Connection_Data            : in out Connection_Data_Type);

   procedure Wait_For_Update_Event (
      Connection_Data            : in out Connection_Data_Type);

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
      Zoom_Direct,
      Zoom_Full,
      Zoom_Inquire
   );

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

   type Options_Type             is array (Index_Type range  <>) of Option_Type;

   type Command_Type             is record
      Length                     : Index_Type;
      Command                    : Maximum_Command_Type;
      Get_Ack                    : Boolean;
      Response_Timeout           : Duration;
      Has_Response               : Boolean;
      Response_Length            : Index_Type;
   end record;

   type Base_Camera_Type         is abstract new General_Camera_Type with private;
   type Base_Camera_Class_Access is access all Base_Camera_Type'class;

   procedure Acked (
      Camera                     : in     Base_Camera_Type;
      Response                   : in     Buffer_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Index_Type) is abstract;

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
      Options                    : in     Options_Type);

   procedure Process_Command (
      Camera                     : in out Base_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type);

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

   Power_On_Preset               : constant := 0;
   Null_Option                   : constant Options_Type;

private

   type Base_Camera_Type         is abstract new General_Camera_Type with record
      Socket                     : Ada_Lib.Socket_IO.Client.Client_Socket_Type;
--    Stream                     : Ada_Lib.Socket_IO.Stream_IO.Stream_Type;
   end record;

   Null_Option                   : constant Options_Type (1 .. 0) :=
                                    ( others => (
                                       Data              => 0,
                                       Start             => 0,
                                       Variable_Width    => False));

end Camera.Lib.Base;


