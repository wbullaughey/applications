with Ada_Lib.Socket_IO;
with Base;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
--with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Section;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Window;
--with Gnoga.Types;
with Widgets.Adjust;
with Widgets.Control;
with Widgets.Configured;
with Widgets.Video;

package Main is

   use type Ada_Lib.Socket_IO.Port_Type;

   Failed                        : exception;

   type Cards_Type;

   type Cards_Access_Type        is access all Cards_Type;

-- type Connection_Data_Type
--                               is new Base.Connection_Data_Type with null record;
--
-- type Connection_Data_Access
--                               is access all Connection_Data_Type;
-- type Connection_Data_Class_Access
--                               is access all Connection_Data_Type'class;
-- type Connection_Data_Constant_Access
--                               is access constant Connection_Data_Type;

   type Navigation_Type          is new Gnoga.Gui.Element.Section.Section_Type
                                    with private;

   function Main_Window (
      Navigation                 : in     Navigation_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type Cards_Type               is new Gnoga.Gui.View.Card.Card_View_Type
                                    with private;

   function Main_Window (
      Cards                      : in     Cards_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type Deck_Type                is new  Gnoga.Gui.View.Docker.Docker_View_Type
                                    with private;

   function Main_Window (
      Deck                       : in     Deck_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type Panel_Type               is new Gnoga.Gui.View.View_Type with private;

   function Main_Window (
      Panel                      : in     Panel_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type Docker_Type              is new  Gnoga.Gui.View.Docker.Docker_View_Type
                                    with private;

   function Main_Window (
      Docker                     : in     Docker_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type View_Type                is new Gnoga.Gui.View.View_Type with private;

   function Main_Window (
      View                       : in     View_Type
   ) return Gnoga.Gui .Base.Pointer_To_Base_Class;

   type Main_Data_Type           is tagged limited private;
   type Main_Data_Access         is access Main_Data_Type;
   type Main_Data_Class_Access   is access all Main_Data_Type'class;

   function Did_Exit (
      Main_Data                  : in     Main_Data_Type
   ) return Boolean;

   function Get_Adjust_Card (
      Connection_Data            : in out Main_Data_Type
   ) return Widgets.Adjust.Adjust_Card_Class_Access;

   function Get_Cards (
      Connection_Data            : in out Main_Data_Type
   ) return Cards_Access_Type;

   function Get_Configured_Card (
      Connection_Data            : in out Main_Data_Type
   ) return Widgets.Configured.Configured_Card_Class_Access;

   function Get_Control_Card (
      Connection_Data            : in out Main_Data_Type
   ) return Widgets.Control.Control_Card_Class_Access;

   function Get_Exit_Button (
      Connection_Data            : in out Main_Data_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class;

   function Get_Main_Window (
      Connection_Data            : in out Main_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class;

   function Get_Tabs (
      Connection_Data            : in out Main_Data_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class;

   function Main_Window (
      Main_Data                  : in     Main_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class;

   procedure Run (
      Directory                  : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
      Verbose                    : in     Boolean;
      Wait_For_Completion        : in     Boolean
   ) with Pre => Port > 0;

   function Running return Boolean;

   Debug                         : Boolean := False;

private

   type Navigation_Type          is new Gnoga.Gui.Element.Section.Section_Type with record
--    Dump_Button                : aliased Gnoga.Gui.Element.Common.Button_Type;
      Exit_Button                : aliased Gnoga.Gui.Element.Common.Button_Type;
--    Load_Button                : aliased Gnoga.Gui.Element.Common.Button_Type;
      Reload_CSS_Button          : aliased Gnoga.Gui.Element.Common.Button_Type;
--    Save_Button                : aliased Gnoga.Gui.Element.Common.Button_Type;
      Trace_Button               : aliased Gnoga.Gui.Element.Common.Button_Type;
   end record;

   type Cards_Type               is new Gnoga.Gui.View.Card.Card_View_Type
                                    with record
      Adjust_Card                : aliased Widgets.Adjust.Adjust_Card_Type;
      Configured_Card            : aliased Widgets.Configured.
                                    Configured_Card_Type;
      Control_Card               : aliased Widgets.Control.Control_Card_Type;
      Video_Card                 : aliased Widgets.Video.Video_Card_Type;
   end Record;

   type Deck_Type                is new  Gnoga.Gui.View.Docker.Docker_View_Type
                                    with record
      Cards                      : aliased Cards_Type;
      Tabs                       : aliased Gnoga.Gui.View.Card.Tab_Type;
   end record;

   type Panel_Type               is new Gnoga.Gui.View.View_Type with record
      Navigation                 : Navigation_Type;
   end record;

   type Docker_Type              is new  Gnoga.Gui.View.Docker.Docker_View_Type
                                    with record
      Deck                       : aliased Deck_Type;
      Panel                      : aliased Panel_Type;
   end Record;

   type View_Type                is new Gnoga.Gui.View.View_Type with record
      Docker                     : Docker_Type;
   end record;

   type Main_Data_Type           is tagged limited record
      Exited                     : Boolean := False;  -- set true by exit button
      GUI_Window                 : Gnoga.Gui.Window.Pointer_To_Window_Class :=
                                    Null;
      View                       : View_Type;
   end record;

end Main;
