with Ada_Lib.Event;
with Ada_Lib.GNOGA;
--with Ada_Lib.Strings;
--with Ada_Lib.Socket_IO.Client;
--with Camera.Command_Queue;
with Camera.Command_Queue;
with Camera.Command_Queue.ALPTOP;
with Camera.Command_Queue.PTZ_Optics;
with Configuration.Camera.State;
--with GNAT.Sockets;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Plugin.Message_Boxes;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Window;
with Main;
with Widgets.Adjust;
with Widgets.Configured;
with Widgets.Control;

package Camera.Lib.Connection is

   Failed                        : exception;

-- use type GNAT.Sockets.Port_Type;

   type Mouse_Click_Action_Type  is (Any_Scroll, Horizontal_Scroll,
                                       No_Action, No_Change, Vertical_Scroll);

   type Connection_Data_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type) is new
                                    Ada_Lib.GNOGA.Connection_Data_Type with record
      Camera_Queue               : Standard.Camera.Command_Queue.Queued_Camera_Class_Access;
      Camera_Pan                 : Absolute_Type;
      Camera_Pan_Speed           : Property_Type;
      Camera_Tilt                : Absolute_Type;
      Camera_Tilt_Speed          : Property_Type;
      Camera_Zoom                : Property_Type;
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
      case Brand is
         when Standard.Camera.Lib.ALPTOP_Camera =>
            ALPTOP                : aliased Standard.Camera.Command_Queue.ALPTOP.
                                    ALPTOP_Type;

         when Standard.Camera.Lib.No_Camera=>
            Null;

         when Standard.Camera.LIB.PTZ_Optics_Camera =>
            PTZ_Optics           : aliased Standard.Camera.Lib.
                                    PTZ_Optics.PTZ_Optics_Type;
      end case;
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

   function Has_Camera_Queue (
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

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

   Debug                         : Boolean := False;

end Camera.Lib.Connection;


