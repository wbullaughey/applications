--with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited;
with AUnit.Test_Suites;
--with Camera.Base;
with Camera.Commands;
with Camera.Configurations;
with Configuration.Camera.State;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.Message_Boxes;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Window;
with Gnoga.Types;
limited with Widgets.Adjust;
limited with Widgets.Control;
limited with Widgets.Configured;

package Camera.Main is

-- use type Ada_Lib.Socket_IO.Port_Type;

   Failed                        : exception;

   type Window_Connection_Type   is abstract new Abstract_Window_Connection_Type with
                                    private;
   type Window_Connection_Access is access all Window_Connection_Type;
   type Window_Connection_Class_Access
                                 is access all Window_Connection_Type'class;

   procedure Allocate_Camera (
      Connection     : in out Window_Connection_Type);

   procedure Close_Message_Box (
      Window_Connection            : in out Window_Connection_Type) is abstract;

   function Did_Exit (
      Window_Connection                  : in     Window_Connection_Type
   ) return Boolean is abstract;

   function Get_Adjust_Card (
      Window_Connection            : in out Window_Connection_Type
   ) return Widgets.Adjust.Adjust_Card_Access is abstract;

-- function Get_Cards (
--    Window_Connection                     : in out Window_Connection_Class_Access
-- ) return Cards_Access_Type;

   function Get_Camera (
      Window_Connection                  : in   Window_Connection_Type
   ) return Commands.Camera_Class_Access is abstract;

--    Camera_ID            : Camera_ID_Type;
--    Camera_Name          : Ada_Lib.Strings.Unlimited.String_Type;
--    Camera_Pan           : Absolute_Type;

   function Get_Camera_Pan_Speed (
      Window_Connection                  : in   Window_Connection_Type
   ) return Property_Type;

--    Camera_Tilt          : Absolute_Type;

   function Get_Camera_Tilt_Speed (
      Window_Connection                  : in   Window_Connection_Type
   ) return Property_Type;

--    Camera_Zoom          : Property_Type;

   -- allocates camera state if 1st time camera is
   function Get_Camera_State (
      Window_Connection                  : in      Window_Connection_Type
   ) return Configuration.Camera.State.State_Access  is abstract;

   function Get_Configured_Card (
      Window_Connection                     : in out Window_Connection_Type
   ) return Widgets.Configured.Configured_Card_Access is abstract;

   function Get_Control_Card (
      Window_Connection                     : in out Window_Connection_Type
   ) return Widgets.Control.Control_Card_Class_Access is abstract;

   function Get_Exit_Button (
      Window_Connection            : in out Window_Connection_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class is abstract;

   function Get_Main_Window (
      Window_Connection            : in out Window_Connection_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is abstract;

   function Get_Tabs (
      Window_Connection            : in out Window_Connection_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class is abstract;

   function Has_Camera_State (
      Window_Connection                  : in     Window_Connection_Type
   ) return Boolean is abstract;

   function Has_Main_Window_Connection
   return Boolean;

-- function Has_Main_Window_Connection_ID
-- return Boolean;
--
   function Main_Window (
      Window_Connection                  : in     Window_Connection_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is abstract;

   procedure Message_Box (
      Window_Connection    : in out Window_Connection_Type;
      Title, Text          : in     String;
      Style                : in     Gnoga.Gui.Plugin.Message_Boxes.
                                       Message_Box_Type :=
                                          Gnoga.Gui.Plugin.
                                             Message_Boxes.OK_Box) is abstract;

   function Mouse_Action (
      Window_Connection            : in   Window_Connection_Type
   ) return Mouse_Click_Action_Type is abstract;

   procedure Reset_Update_Event (
      Window_Connection            : in out Window_Connection_Type) is abstract;

-- function Shared_Connection (
--    Window_Connection            : in   Window_Connection_Type
-- ) return Base.Camera_State_Class_Access is abstract;

   procedure Set_Main_Created (
      Window_Connection            : in out Window_Connection_Type;
      Value                         : in     Boolean) is abstract;

-- overriding
-- procedure Set_Connection_Data_Main_Window (
--    Window_Connection : in out Window_Connection_Type;
--    Main_Window       : in     Gnoga.Gui.Window.
--                                  Pointer_To_Window_Class) is abstract;
-- procedure Set_Mouse_Action (
--    Window_Connection       : in out Window_Connection_Type;
--    Action                  : in     Camera.Mouse_Click_Action_Type) is abstract;
--
-- procedure Set_Preset (
--    Window_Connection             : in out Window_Connection_Type;
--    Preset                        : in     Preset_ID_Type) is abstract;

   procedure Trigger_Update_Event (
      Window_Connection            : in out Window_Connection_Type) is abstract;

   procedure Wait_For_Update_Event (
      Window_Connection            : in out Window_Connection_Type) is abstract;

   subtype Window_ID_Type        is Gnoga.Types.Connection_ID;

   function Allocate_Window_Connection
   return Window_Connection_Class_Access;

   function Get_Current_Connection
   return Window_Connection_Class_Access;

   --  Setup GUI for each connection.
   procedure On_Connect (
      Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Gnoga.Application.Multi_Connect.
                                             Connection_Holder_Type
   ) with Pre => Configurations.Has_Configuration;

   procedure Open_Camera (
      Connection     : in out Window_Connection_Type;
      Description    : in     Ada_Lib.Strings.String_Constant_Access);

   overriding
   procedure Process_Command (
      Connection_Data            : in out Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Timeout_Time               : in     Duration := 0.0) is abstract;
                                          -- when 0 use command default
   overriding
   procedure Process_Command (
      Connection_Data            : in out Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0)  is abstract;
                                          -- when 0 use command default

   procedure Set_Mouse_Action (
      Connection_Data            : in out Window_Connection_Type;
      Action                     : in     Mouse_Click_Action_Type);

   function Running return Boolean;

   function Unit_Test_Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Window_Connection_Type   is abstract new Abstract_Window_Connection_Type with
                                    record
      Camera               : Commands.Camera_Class_Access := Null;
      Camera_ID            : Camera_ID_Type;
      Camera_Name          : Ada_Lib.Strings.Unlimited.String_Type;
      Camera_Pan           : Absolute_Type;
      Camera_Pan_Speed     : Property_Type;
      Camera_Tilt          : Absolute_Type;
      Camera_Tilt_Speed    : Property_Type;
      Camera_Zoom          : Property_Type;
      Mouse_Action         : Mouse_Click_Action_Type;
   end record;

end Camera.Main;
