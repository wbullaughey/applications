--with Ada_Lib.Event;
--with Ada_Lib.GNOGA;
with Ada_Lib.Socket_IO.Client;
--with Camera.Command_Queue;
--with Configuration.Camera; -- .State;
with GNAT.Sockets;
--with Gnoga.Gui.Element.Common;
--with Gnoga.Gui.Plugin.jQueryUI.Widget;
--with Gnoga.Gui.Plugin.Message_Boxes;
--with Gnoga.Gui.View.Card;
--with Gnoga.Gui.Window;
--with Main;
--with Widgets.Adjust;
--with Widgets.Configured;
--with Widgets.Control;

package Camera.Lib.Base is

   Failed                        : exception;
   Timed_Out                     : exception;

   use type GNAT.Sockets.Port_Type;

   type Command_Type             is record
      Length                     : Index_Type;
      Command                    : Maximum_Command_Type;
      Get_Ack                    : Ack_Response_Type;
      Response_Timeout           : Duration;
      Has_Response               : Boolean;
      Response_Length            : Index_Type;
   end record;

   procedure Check_Command (
      Command_Code               : in     Commands_Type;
      Command                    : in     Command_Type);

   type Base_Camera_Type         is limited new General_Camera_Type with private;
   type Base_Camera_Class_Access is access all Base_Camera_Type'class;

-- procedure Acked (
--    Camera                     : in     Base_Camera_Type;
--    Response                   : in     Response_Type;
--    Value                      :    out Natural;
--    Next_Buffer_Index          :    out Index_Type) is abstract;

   overriding
   procedure Close (
      Camera                     : in out Base_Camera_Type);

-- procedure Completed (
--    Camera                     : in     Base_Camera_Type;
--    Buffer                     : in     Response_Type;
--    Start                      : in     Index_Type;
--    Completion_Value           :    out Natural;
--    Next_Byte                  :    out Index_Type) is abstract;

-- function Get_Ack_Length (
--    Camera                     : in     Base_Camera_Type
-- ) return Index_Type is abstract;

-- procedure Get_Absolute (
--    Camera                     : in out Base_Camera_Type;
--    Pan                        :    out Absolute_Type;
--    Tilt                       :    out Absolute_Type) is abstract;

-- function Get_Default_Preset (
--    Camera                     : in     Base_Camera_Type
-- ) return Configuration.Camera.Preset_ID_Type is abstract;

-- function Get_Timeout (
--    Camera                     : in     Base_Camera_Type;
--    Command                    : in     Commands_Type
-- ) return Duration is abstract;

-- procedure Get_Zoom (
--    Camera                     : in out Base_Camera_Type;
--    Zoom                       :    out Absolute_Type) is abstract;

   overriding
   procedure Host_Open (
      Camera                     :    out Base_Camera_Type;
      Host_Address               : in     String;
      Port                       : in     GNAT.Sockets.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0);

   overriding
   procedure IP_Open (
      Camera                     :    out Base_Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     GNAT.Sockets.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0
   ) with Pre => Port /= 0;

   overriding
   procedure Open (
      Camera                     :    out Base_Camera_Type;
      Address                    : in     Ada_Lib.Socket_IO.Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0);

-- procedure Position_Relative (
--    Camera                     : in out Base_Camera_Type;
--    Pan                        : in      Relative_Type;
--    Tilt                       : in      Relative_Type;
--    Pan_Speed                  : in      Property_Type := 1;
--    Tilt_Speed                 : in      Property_Type := 1) is abstract;

-- procedure Process_Command (
--    Camera                     : in out Base_Camera_Type;
--    Command                    : in     Commands_Type;
--    Options                    : in     Options_Type);
--
-- procedure Process_Command (
--    Camera                     : in out Base_Camera_Type;
--    Command                    : in     Commands_Type;
--    Options                    : in     Options_Type;
--    Response                   :    out Maximum_Response_Type;
--    Response_Length            :    out Index_Type);

-- procedure Process_Response (
--    Camera                     : in     Base_Camera_Type;
--    Response                   : in     Response_Type;
--    Value                      :    out Data_Type;
--    Next_Buffer_Start          :    out Index_Type) is abstract;

-- procedure Send_Command (
--    Camera                     : in out Base_Camera_Type;
--    Command                    : in     Commands_Type;
--    Get_Ack                    :    out Boolean;
--    Has_Response               :    out Boolean;
--    Response_Length            :    out Index_Type) is abstract;

-- procedure Read (
--    Camera                     :    out Base_Camera_Type;
--    Data                       :    out Data_Type;
--    Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Read (
      Camera                     : in out Base_Camera_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Reopen (
      Camera                     : in out Base_Camera_Type;
      Address                    : in     Ada_Lib.Socket_IO.Address_Type;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type);

-- procedure Send_Command (
--    Camera                     : in out Base_Camera_Type;
--    Command                    : in     Commands_Type;
--    Options                    : in     Options_Type;
--    Get_Ack                    :    out Boolean;
--    Has_Response               :    out Boolean;
--    Response_Length            :    out Index_Type) is abstract;

-- procedure Set_Absolute (
--    Camera                     : in out Base_Camera_Type;
--    Pan                        : in     Absolute_Type;
--    Tilt                       : in     Absolute_Type;
--    Pan_Speed                  : in     Property_Type := 1;
--    Tilt_Speed                 : in     Property_Type := 1) is abstract;
--
-- procedure Set_Power (
--    Camera                     : in out Base_Camera_Type;
--    On                         : in     Boolean) is abstract;

   -- updates preset to current location
-- procedure Update_Preset (
--    Camera                     : in out Base_Camera_Type;
--    Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
--    Wait_Until_Finished        : in     Boolean := True) is abstract;

   overriding
   procedure URL_Open (
      Camera                     :    out Base_Camera_Type;
      URL                        : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type);

-- procedure Write (
--    Camera                     :    out Base_Camera_Type;
--    Data                       : in     Data_Type);

   procedure Write (
      Camera                     : in out Base_Camera_Type;
      Data                       : in     Buffer_Type);

   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Options                    : in     Options_Type);

   procedure Apply_Parameters (
      Buffer                     : in out Maximum_Command_Type;
      Command                    : in     Response_Type;
      Options                    : in     Options_Type);

   Debug                         : Boolean := False;
   Last_Command                  : constant := 127;
   Power_On_Preset               : constant := 0;

private

   Camera_Description            : aliased constant String := "camera socket";

   type Base_Camera_Type   is limited new General_Camera_Type with record
      Last_Command               : Commands_Type := No_Command;
      Socket                     : Ada_Lib.Socket_IO.Client.Client_Socket_Type (
                                       Camera_Description'access);
--    Waiting_For_Response       : Boolean := False;
   end record;

end Camera.Lib.Base;


