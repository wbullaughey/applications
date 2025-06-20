with Ada_Lib.Event;
--with Ada_Lib.GNOGA;
with Camera.Commands;
with Configuration.Camera.State;
with GNOGA_Ada_Lib;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Plugin.Message_Boxes;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Window;
with Main;
with Widgets.Adjust;
with Widgets.Configured;
with Widgets.Control;

package Base is

   Failed                        : exception;

   type Mouse_Click_Action_Type  is (Any_Scroll, Horizontal_Scroll,
                                       No_Action, No_Change, Vertical_Scroll);

   type Connection_Data_Type     is new GNOGA_Ada_Lib.Connection_Data_Type with
                                    record
      Camera                     : Standard.Camera.Commands.Camera_Class_Access :=
                                    Null;
      Camera_Pan                 : Standard.Camera.Commands.Absolute_Type;
      Camera_Pan_Speed           : Standard.Camera.Commands.Property_Type;
      Camera_Tilt                : Standard.Camera.Commands.Absolute_Type;
      Camera_Tilt_Speed          : Standard.Camera.Commands.Property_Type;
      Camera_Zoom                : Standard.Camera.Commands.Property_Type;
      Main_Data                  : Main.Main_Data_Access;
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

   Debug                         : Boolean := False;


end Base;
