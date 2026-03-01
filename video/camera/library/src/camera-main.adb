with Ada_Lib.Event;
with Ada.Exceptions;
--with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Help;
with Ada_Lib.Options;
with Ada_Lib.OS;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ask;
with Camera.Base;
--with Camera.Configurations;
with Camera.Lib.Base;
with Camera.Lib.Options;
--with Configuration.Camera.State;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Section;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.View.Docker;
with GNAT.Sockets;
with Gnoga.Types.Colors;
with Widgets.Adjust;
with Widgets.Control; -- causes hang
with Widgets.Configured;
with Widgets.Video;

package body Camera.Main is

-- use type Base.Connection_Data_Class_Access;
   use type Configuration.Camera.State.State_Access;
   use type Gnoga.Types.Pointer_to_Connection_Data_Class;
   use type GNOGA.GUI.Plugin.JQueryui.Widget.Dialog_Access;
-- use type Base.Camera_State_Class_Access;

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
      Control_Card               : Widgets.Control.Control_Card_Class_Access;
                                 -- changed to access to avoid compiler bug
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

   type Full_Window_Connection_Type
                           is new Window_Connection_Type with record
      Camera_State         : Configuration.Camera.State.State_Access := Null;
      Exited               : Boolean := False;  -- set true by exit button
      GUI_Window           : Gnoga.Gui.Window.Pointer_To_Window_Class :=
                              Null;
      Main_Created         : Boolean := False;
--    Main_Window          : Gnoga.Gui.Window.Pointer_To_Window_Class := Null;
      Message_Box_Dialog   : Gnoga.Gui.Plugin.jQueryUI.Widget.
                              Dialog_Access := Null;
      Message_Box_Result   : Gnoga.Gui.Plugin.Message_Boxes.
                              Message_Box_Result;
      Update_Event         : Ada_Lib.Event.Event_Type (
                              new String'("update event"));
      View                 : View_Type;
   end record;

   type Full_Window_Connection_Access
      is access Full_Window_Connection_Type;
   type Full_Window_Connection_Class_Access
      is access all Full_Window_Connection_Type'class;

   overriding
   procedure Close_Message_Box (
      Window_Connection            : in out Full_Window_Connection_Type);

-- function Connection_Data_Equal (
--    Left, Right                   : in     Window_Connection_Access
-- ) return Boolean;

   overriding
   function Did_Exit (
      Window_Connection       : in     Full_Window_Connection_Type
   ) return Boolean;

   procedure Exit_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

-- procedure Exit_Mouse_Button_Click_Handler (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
--    Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record
-- ) with Pre => Object.Connection_Data /= Null;

   --  Application event handlers
-- procedure On_Exit (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class);

   overriding
   function Get_Adjust_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Adjust.Adjust_Card_Access;

   overriding
   function Get_Camera (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Commands.Camera_Class_Access;

   overriding
   function Get_Camera_State (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Configuration.Camera.State.State_Access
   with Pre    => Window_Connection.Has_Camera_State;

   overriding
   function Get_Control_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Control.Control_Card_Class_Access;

   overriding
   function Get_Configured_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Configured.Configured_Card_Access;

-- --  Setup another path in to the application for submitting results
-- --  /result, see On_Connect_Handler in body of this procedure.
-- procedure On_Result_Connect (
--    Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
--    Connection                 : access Gnoga.Application.Multi_Connect.
--                                           Connection_Holder_Type);

   overriding
   function Get_Exit_Button (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class;

   overriding
   function Get_Main_Window (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class;

   overriding
   function Get_Tabs (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class;

   overriding
   function Has_Camera_State (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Boolean;

   overriding
   function Main_Window (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class;

   overriding
   procedure Message_Box (
      Window_Connection            : in out Full_Window_Connection_Type;
      Title, Text                : in     String;
      Style                      : in     Gnoga.Gui.Plugin.Message_Boxes.
                                             Message_Box_Type :=
                                             Gnoga.Gui.Plugin.Message_Boxes.OK_Box);

   overriding
   function Mouse_Action (
      Window_Connection            : in   Full_Window_Connection_Type
   ) return Mouse_Click_Action_Type;

   overriding
   procedure Process_Command (
      Connection_Data            : in out Full_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Timeout_Time               : in     Duration := 0.0);

   overriding
   procedure Process_Command (
      Connection_Data            : in out Full_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0);

   procedure Reload_CSS_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

   overriding
   procedure Reset_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type);

   overriding
   procedure Set_Main_Created (
      Window_Connection          : in out Full_Window_Connection_Type;
      Value                      : in     Boolean);

-- procedure Set_Main_Window (
--    Window_Connection          : in out Full_Window_Connection_Type;
--    Value                      : in     Boolean);

-- overriding
-- procedure  Set_Connection_Data_Main_Window (
--    Window_Connection         : in out Full_Window_Connection_Type;
--    Main_Window             : in     Gnoga.Gui.Window.
--                                        Pointer_To_Window_Class);

   overriding
   procedure Trigger_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type);

   overriding
   procedure Wait_For_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type);

-- function Window_ID_Equal (
--    Left, Right                : in     Window_ID_Type
-- ) return Boolean;
--
-- function Window_ID_Hash (
--    Key                        : in     Window_ID_Type
-- ) return Ada.Containers.Hash_Type;

   procedure Trace_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

   package Unit_Test is

      function Suite return AUnit.Test_Suites.Access_Test_Suite;

   end Unit_Test;

-- package Widgets_Configured_Unit_Test is
--
--    function Suite return AUnit.Test_Suites.Access_Test_Suite;
--
-- end Widgets_Configured_Unit_Test;

   package body Unit_Test is separate;

   Debug    : Boolean renames Camera.Lib.Options.Camera_Options.Main_Debug;
   Description                   : aliased constant String := "main camera";
   Main_Window_Connection_ID     : constant Gnoga.Types.Connection_ID :=
                                     Gnoga.Types.No_Connection;
   Started                       : Boolean := False;

   ---------------------------------------------------------------
   function Allocate_Window_Connection
   return Window_Connection_Class_Access is
   ---------------------------------------------------------------

   begin
      return Window_Connection_Class_Access'(new Full_Window_Connection_Type);
   end Allocate_Window_Connection;

   ---------------------------------------------------------------
   overriding
   procedure Close_Message_Box (
      Window_Connection            : in out Full_Window_Connection_Type) is
   ---------------------------------------------------------------

      Message  : constant String := "message box dooes not exist";

   begin
      Log_In (Debug);
      if Window_Connection.Message_Box_Dialog = Null then
         Log_Here (Debug, Message);
         Log_Exception (Debug, Message);
         raise Failed with Message;
      else
         Window_Connection.Message_Box_Dialog.Fire_On_Close;
      end if;
      Log_Out (Debug);
   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug, Fault);
         raise;

   end Close_Message_Box;

   ---------------------------------------------------------------
   function Connection_Data_Equal (
      Left, Right                : in     Window_Connection_Access
   ) return Boolean is
   ---------------------------------------------------------------

   begin
not_implemented;
return false;
   end Connection_Data_Equal;

-- ---------------------------------------------------------------
-- procedure Close_Message_Box (
--    Connection_Data            : in out Connection_Data_Type) is
-- ---------------------------------------------------------------
--
--    Message                    : constant String :=
--                                  "message box dooes not exist";
--
-- begin
--    Log_In (Debug);
--    if Connection_Data.Local_Connection.Message_Box_Dialog = Null then
--       Log_Here (Debug, Message);
--       Log_Exception (Debug, Message);
--       raise Failed with Message;
--    else
--       Connection_Data.Local_Connection.Message_Box_Dialog.Fire_On_Close;
--    end if;
--    Log_Out (Debug);
-- exception
--
--    when Fault: others =>
--       Trace_Exception (Debug, Fault);
--       Log_Exception (Debug, Fault);
--       raise;
--
-- end Close_Message_Box;

   ---------------------------------------------------------------
   -- create the navigation buttons: exit delete all, delete unsubscribed, dump, reload css, trace
   procedure Create (
      Navigation                 : in out Navigation_Type;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, " create buttons");
      Navigation.Create (Parent, Gnoga.Gui.Element.Section.Nav,
         ID => "Navigation");
      Navigation.Exit_Button.Create (Navigation, "Exit", "exit_button");
      Navigation.Exit_Button.On_Click_Handler (
        Exit_Button_Click_Handler'Access);

      Navigation.Reload_CSS_Button.Create (Navigation, "Reload CSS");
      Navigation.Reload_CSS_Button.On_Click_Handler (
         Reload_CSS_Button_Click_Handler'Access);

      Navigation.Trace_Button.Create (Navigation, "Trace");
      Navigation.Trace_Button.On_Click_Handler (
         Trace_Button_Click_Handler'Access);
      Log_Out (Debug);
   end Create;

   ---------------------------------------------------------------
   overriding
   function Did_Exit (
      Window_Connection       : in     Full_Window_Connection_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Window_Connection.Exited;
   end Did_Exit;

   ---------------------------------------------------------------
   procedure Exit_Button_Click (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

      Full_Window_Connection  : Full_Window_Connection_Type renames
                                 Full_Window_Connection_Type (
                                    Object.Connection_Data.all);
   begin
      Log_In (Debug);
--    if Connection_Data = Null then   -- GNOGA already closed
--       Log_Out (Debug);
--       return;
--    end if;

      declare
         View        : View_Type renames Full_Window_Connection.View;
         Docker      : Docker_Type renames View.Docker;
         Panel       : Panel_Type renames Docker.Panel;
         Navigation  : Navigation_Type renames Panel.Navigation;

      begin
         Full_Window_Connection.Exited := True;
         Navigation.Exit_Button.Disabled;
         Panel.Put_Line ("Closing application");
         Gnoga.Application.Multi_Connect.End_Application;
      end;
      Log_Out (Debug);
   end Exit_Button_Click;

   ---------------------------------------------------------------
   procedure Exit_Button_Click_Handler (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Exit_Button_Click (Object);
      Log_Out (Debug);
   end Exit_Button_Click_Handler;

-- ----------------------------------------------------------------
-- procedure Exit_Mouse_Button_Click_Handler (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
--    Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record) is
-- pragma Unreferenced (Mouse_Event);
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Exit_Button_Click (Object);
--    Log_Out (Debug);
-- end Exit_Mouse_Button_Click_Handler;

-- ----------------------------------------------------------------
-- function Get_Default_Speed (
--    Base_Data            : in     Base_Data_Type
-- ) return Camera.Commands.Property_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Camera.Commands.Property_Type (
--       Base_Data.State.Default_Speed);
-- end Get_Default_Speed;

   ----------------------------------------------------------------
   overriding
   function Get_Adjust_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Adjust.Adjust_Card_Access is
   ----------------------------------------------------------------

   begin
      return Window_Connection.View.Docker.
         Deck.Cards.Adjust_Card'unchecked_access;
   end Get_Adjust_Card;

   ---------------------------------------------------------------
   overriding
   function Get_Camera (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Commands.Camera_Class_Access is
   ---------------------------------------------------------------

   begin
not_implemented;
return null;
--    return Window_Connection.Get_Camera;
   end Get_Camera;

   ---------------------------------------------------------------
   function Get_Camera_Pan_Speed (
      Window_Connection                  : in   Window_Connection_Type
   ) return Property_Type is
   ---------------------------------------------------------------

   begin
      return Window_Connection.Camera_Pan_Speed;
   end Get_Camera_Pan_Speed;

   ---------------------------------------------------------------
   overriding
   function Get_Camera_State (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Configuration.Camera.State.State_Access is
   ---------------------------------------------------------------

   begin
      return Window_Connection.Camera_State;
   end Get_Camera_State;

   ---------------------------------------------------------------
   function Get_Camera_Tilt_Speed (
      Window_Connection                  : in   Window_Connection_Type
   ) return Property_Type is
   ---------------------------------------------------------------

   begin
      return Window_Connection.Camera_Tilt_Speed;
   end Get_Camera_Tilt_Speed;

-- ----------------------------------------------------------------
-- function Get_Cards (
--    Window_Connection                     : in out Full_Window_Connection_Type
-- ) return Cards_Access_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Window_Connection.View.Docker.Deck.Cards'unchecked_access;
-- end Get_Cards;

   ----------------------------------------------------------------
   overriding
   function Get_Configured_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Configured.Configured_Card_Access is
   ----------------------------------------------------------------

   begin
      return Window_Connection.View.
         Docker.Deck.Cards.Configured_Card'unchecked_access;
   end Get_Configured_Card;

   ----------------------------------------------------------------
   overriding
   function Get_Control_Card (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Widgets.Control.Control_Card_Class_Access is
   ----------------------------------------------------------------

   begin
      return Window_Connection.View.Docker.Deck.Cards.Control_Card;
   end Get_Control_Card;

   ----------------------------------------------------------------
   function Get_Current_Connection
   return Window_Connection_Class_Access is
   ----------------------------------------------------------------

   begin
not_Implemented;
return Null;
   end Get_Current_Connection;

   ----------------------------------------------------------------
   overriding
   function Get_Exit_Button (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class is
   ----------------------------------------------------------------

   begin
      return Window_Connection.View.Docker.Panel.Navigation.
         Exit_Button'unchecked_access;
   end Get_Exit_Button;

   ----------------------------------------------------------------
   overriding
   function Get_Main_Window (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
   ----------------------------------------------------------------

   begin
      return Window_Connection.GUI_Window;
   end Get_Main_Window;

   ----------------------------------------------------------------
   overriding
   function Get_Tabs (
      Window_Connection                     : in out Full_Window_Connection_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class is
   ----------------------------------------------------------------

   begin
      return Window_Connection.View.Docker.Deck.Tabs'unchecked_access;
   end Get_Tabs;

   ---------------------------------------------------------------
   overriding
   function Has_Camera_State (
      Window_Connection                  : in     Full_Window_Connection_Type
   ) return Boolean is
   ---------------------------------------------------------------

      Result   : constant Boolean :=
                              Window_Connection.Camera_State /= Null;
   begin
      return Log_Here (Result,
         Trace_Pre_Post_Conditions or not Result, "Camera state not set");
   end Has_Camera_State;

   ---------------------------------------------------------------
   function Has_Main_Window_Connection
   return Boolean is

   begin
not_implemented;
return false;
   end Has_Main_Window_Connection;

   ---------------------------------------------------------------
   function Has_Main_Window_Connection_ID
   return Boolean is
   ---------------------------------------------------------------

   begin
      return Main_Window_Connection_ID /= Gnoga.Types.No_Connection;
   end Has_Main_Window_Connection_ID;

-- ----------------------------------------------------------------
-- function Main_Window (
--    Navigation                 : in     Navigation_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Navigation.Parent.Parent.Parent.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- function Main_Window (
--    Cards                      : in     Cards_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Cards.Parent.Parent.Parent.Parent.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- function Main_Window (
--    Deck                       : in     Deck_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Deck.Parent.Parent.Parent.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- function Main_Window (
--    Panel                      : in     Panel_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Panel.Parent.Parent.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- function Main_Window (
--    Docker                     : in     Docker_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Docker.Parent.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- function Main_Window (
--    View                       : in     View_Type
-- ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return View.Parent;
-- end Main_Window;
--
-- ----------------------------------------------------------------
-- overriding
-- function Main_Window (
--    Window_Connection                  : in     Full_Window_Connection_Type
-- ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
-- ----------------------------------------------------------------
--
-- begin
--    return Window_Connection.GUI_Window;
-- end Main_Window;
--
   ----------------------------------------------------------------
   overriding
   procedure Message_Box (
      Window_Connection            : in out Full_Window_Connection_Type;
      Title, Text                : in     String;
      Style                      : in     Gnoga.Gui.Plugin.Message_Boxes.
                                             Message_Box_Type :=
                                             Gnoga.Gui.Plugin.Message_Boxes.OK_Box) is
   ----------------------------------------------------------------

      Message_Box_Dialog         : aliased Gnoga.Gui.Plugin.jQueryUI.
                                    Widget.Dialog_Type;

   begin
      Log_In (Debug, Quote ("text", Text));
      Window_Connection.Message_Box_Dialog := Message_Box_Dialog'unchecked_access;
      Window_Connection.Message_Box_Result :=
         Gnoga.Gui.Plugin.Message_Boxes.Message_Box (
            Message_Box_Dialog, -- Window_Connection.Main_Window.all,
            Title, Text, Style);
      Log_Out (Debug, "result " & Window_Connection.Message_Box_Result'img);
      Window_Connection.Message_Box_Dialog := Null;
   end Message_Box;

   ----------------------------------------------------------------
   overriding
   function Mouse_Action (
      Window_Connection            : in   Full_Window_Connection_Type
   ) return Mouse_Click_Action_Type is
   ----------------------------------------------------------------

   begin
      return Window_Connection.Mouse_Action;
   end Mouse_Action;

   ----------------------------------------------------------------
   procedure On_Connect (
      Main_Window          : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection           : access Gnoga.Application.Multi_Connect.
                                       Connection_Holder_Type) is
   pragma Unreferenced (Connection);
   ----------------------------------------------------------------

      State_Pointer  : constant Configurations.
                        Camera_Configuration_State_Constant_Class_Access :=
                           Camera.Configurations.Get_Read_Only_Camera_Configuration_State;

      State    : Configuration.Camera.State.State_Type'class renames
                  State_Pointer.all;
   begin
      Log_In (Debug, "started " & Started'img &
         " connection id" & Main_Window.Connection_ID'img);
--       " main window " & Image (Main_Window'address) &
--       " connection data " & Image (Connection_Data'address));

     Ada_Lib.GNOGA.Set_Main_Window (Main_Window'unchecked_access);
     Started := True;

     declare
        Full_Window_Connection : constant Full_Window_Connection_Access :=
                             new Full_Window_Connection_Type;
--      State             : Camera.Base.Camera_State_Access :=
--                            Camera.Base.Allocate_Camera_State (
--                               Camera_ID (Read_Only_Global_Camera_State.
--                                  Video_Address.IP_Address));
        View              : View_Type renames Full_Window_Connection.all.View;
        Docker            : Docker_Type renames View.Docker;
        Deck              : Deck_Type renames Docker.Deck;
        Panel             : Panel_Type renames Docker.Panel;
        Navigation        : Navigation_Type renames Panel.Navigation;
        Cards             : Cards_Type renames Deck.Cards;
        Tabs              : Gnoga.Gui.View.Card.Tab_Type renames Deck.Tabs;
        Adjust_Card       : Widgets.Adjust.Adjust_Card_Type renames
                             Cards.Adjust_Card;
        Configured_Card   : Widgets.Configured.Configured_Card_Type renames
                             Cards.Configured_Card;
        Control_Card      : constant Widgets.Control.Control_Card_Class_Access :=
                              Widgets.Control.Allocate_Control_Card;
        Video_Card        : Widgets.Video.Video_Card_Type renames
                             Cards.Video_Card;

     begin
        Cards.Control_Card := Control_Card;
        Full_Window_Connection.Open_Camera (Description'access);

        Main_Window.Connection_Data (
           Gnoga.Types.Pointer_to_Connection_Data_Class (
              Full_Window_Connection));
        Full_Window_Connection.Main_Window := Main_Window'unchecked_access;
        View.Create (Main_Window); --, ID => "Main View");

        declare
           CSS_Path             : constant String :=
                                   State.Get_CSS_Path;
        begin
           Log_Here (Debug, Quote ("class path", CSS_Path));
--          Main_Window.Load_CSS_File (CSS_Path);
           View.Load_CSS (CSS_Path);
           Log_Here (Debug);

        exception
           when Fault: Ada.IO_Exceptions.Device_Error =>
              Trace_Exception (Fault);
              raise Failed with "could not load CSS file " & CSS_Path;
        end;

        Full_Window_Connection.GUI_Window := Gnoga.Gui.Window.Pointer_To_Window_Class'(
           Main_Window'unchecked_access);

        Log_Here (Debug, " create docker");
        Docker.Create (View, ID => "Docker");

        Log_Here (Debug, " create pannel");
        Panel.Create (Docker, ID => "Panel");
        Panel.Background_Color ("silver");

        Log_Here (Debug, " create navigation");
        Navigation.Create (Panel);
        Docker.Top_Dock (Gnoga.Gui.Element.Pointer_To_Element_Class'(
           Panel'Unchecked_Access));

        Log_Here (Debug, " create deck");
        -- create a deck to put cards in
        Deck.Create (Docker, ID => "Deck");
        -- put the deck in the docker
        Docker.Fill_Dock (Gnoga.Gui.Element.Pointer_To_Element_Class'(
           Deck'Unchecked_Access));

        Log_Here (Debug, " create cards " & Image (Cards'address));
        Cards.Create (Deck, ID => "Cards");
        Cards.Border;

        Log_Here (Debug, "fill deck");
        Deck.Fill_Dock (Gnoga.Gui.Element.Pointer_To_Element_Class'(
           Cards'Unchecked_Access));

        Log_Here (Debug, " create Tabs");
        Tabs.Create (
           Parent         => Deck,
           Card_View      => Cards,
           ID             => "Tabs",
           Select_Color   => Gnoga.Types.Colors.Black,
           Tab_Color      => Gnoga.Types.Colors.Gray);
        Log_Here (Debug, " create Control_Card");

        Control_Card.Create (
           Main_Window    => Main_Window,
           Parent         => Cards,
           ID             => "");
        Control_Card.Class_Name; -- (Configuration.Camera.Control_Card_Style);

        Log_Here (Debug, " add Control_Card card");
        Cards.Add_Card (
           Name => Widgets.Control.Widget_Name,
           Card => Control_Card.Get_Card);

        Log_Here (Debug, "add Control_Card tab " & Quote ("widget name", Widgets.Control.Widget_Name));
        Tabs.Add_Tab (Widgets.Control.Widget_Name,
           Widgets.Control.Widget_Name, Selected => True);

        Log_Here (Debug, " create Adjust_Card");
--       Adjust_Card.Create (Main_Window, Cards);
        Widgets.Adjust.Create (Adjust_Card, Main_Window, Cards);

        Log_Here (Debug, " add Adjust_Card card");
        Cards.Add_Card (
           Name => Widgets.Adjust.Widget_Name,
           Card => Adjust_Card'access);

        Log_Here (Debug, " add Adjust_Card tab " & image (Tabs'address));
        Tabs.Add_Tab (Widgets.Adjust.Widget_Name,
           Widgets.Adjust.Widget_Name);

        Log_Here (Debug, " create Configured_Card");
--       Configured_Card.Create (Main_Window, Cards);
        Widgets.Configured.Create (Configured_Card, Main_Window, Cards);

        Log_Here (Debug, " add Configured_Card card");
        Cards.Add_Card (
           Name => Widgets.Configured.Widget_Name,
           Card => Configured_Card'access);

        Log_Here (Debug, " add Configured_Card tab " & image (Tabs'address));
        Tabs.Add_Tab (Widgets.Configured.Widget_Name,
           Widgets.Configured.Widget_Name);

        Log_Here (Debug, " create video");
        Video_Card.Create (Main_Window, Cards);

        Log_Here (Debug, " add Video_Card card");
        Cards.Add_Card (
           Name => Widgets.Video.Widget_Name,
           Card => Video_Card'access);

        Log_Here (Debug, " add Video_Card tab");
        Tabs.Add_Tab (Widgets.Video.Widget_Name,
           Widgets.Video.Widget_Name);

        Log_Here (Debug, " set View dock");
        Deck.Top_Dock (Gnoga.Gui.Element.Pointer_To_Element_Class'(
           Tabs'Unchecked_Access));
        Cards.Top (
           Value    => 45,
           Unit     => "px");
--       Tabs.Select_Tab (Widgets.Control.Widget_Name);
--       Panel.Visible (True);
        Full_Window_Connection.Main_Created := True;
     end;

--    Camera.Run.Set_Base (new Main_Base_Type);
-- pause ("end of on_connect");
      Log_Out (Debug);

  exception
     when Fault: Standard.Camera.Lib.Base.Failed =>
        Put_Line (Ada.Exceptions.Exception_Message (Fault));
        Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);

     when Fault: others =>
        Trace_Exception (Fault, Here);
        Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_such_file_or_directory);

   end On_Connect;

-- ----------------------------------------------------------------
-- procedure On_Exit (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class) is
-- pragma Unreferenced (Object);
-- ----------------------------------------------------------------
-- begin
--    Log_In (Debug);
--    Gnoga.Application.Multi_Connect.End_Application;
-- end On_Exit;

-- ----------------------------------------------------------------
-- procedure On_Result_Connect (
--    Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
--    Connection                 : access Gnoga.Application.Multi_Connect.
--                                           Connection_Holder_Type) is
-- ----------------------------------------------------------------
--    pragma Unreferenced (Connection);
--    --  Since there will be no interactions with page once displayed there
--    --  is no need to setup any data to associate with the main window.
--
--    Result_View : constant Gnoga.Gui.View.View_Access :=
--      new Gnoga.Gui.View.View_Type;
-- begin
--    Result_View.Dynamic;
--    --  By marking the View dynamic it will be deallocated by Main_Window
--    --  when it finalizes.
--    Result_View.Create (Main_Window);
--
--    Result_View.Put_Line ("Name : " & Main_Window.Form_Parameter ("Name"));
--    Result_View.Put_Line ("Message : " &
--                            Main_Window.Form_Parameter ("Message"));
-- end On_Result_Connect;

   ---------------------------------------------------------------
   procedure Open_Camera (
      Connection     : in out Window_Connection_Type;
      Description    : in     Ada_Lib.Strings.String_Constant_Access) is
   ---------------------------------------------------------------

      State       : Configuration.Camera.State.State_Type'class
                     renames Configurations.
                        Get_Read_Only_Camera_Configuration_State (
                           Connection.Camera_ID).all;
      Port_Number : constant Standard.Camera.Port_Type :=
                         State.Get_Host_Port;
      Camera_Address  : constant Ada_Lib.Socket_IO.Address_Type :=
                         State.Get_Host_Address;
   begin
      Log_In (Debug,
         Quote (" Camera_URL", Camera_Address.Image) &
         " port" & Port_Number'img);

not_implemented;  -- need way to call camera allocator based on configuration 2//26/26
--    Connection.Camera :=
--       Standard.Camera.Commands.Camera_Class_Access'(
--          new Standard.Camera.Commands.PTZ_Optics.PTZ_Optics_Type (
--             Description));

      Connection.Camera.Open (Camera_Address, Port_Number);
      Log_Out (Debug);

   exception

      when Fault: GNAT.Sockets.Host_Error =>
         Trace_Exception (Debug, Fault, Here);
         Put_Line ("Could not open camera. Error " &
            Ada.Exceptions.Exception_Message (Fault));
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);

   end Open_Camera;

   ---------------------------------------------------------------
   overriding
   procedure Process_Command (
      Connection_Data            : in out Full_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Timeout_Time               : in     Duration := 0.0) is
                                          -- when 0 use command default
   ---------------------------------------------------------------

   begin
not_implemented;
   end Process_Command;

   ---------------------------------------------------------------
   overriding
   procedure Process_Command (
      Connection_Data            : in out Full_Window_Connection_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Command_Options_Type;
      Response                   :    out Maximum_Response_Type;
      Timeout_Time               : in     Duration := 0.0) is
                                          -- when 0 use command default
   ---------------------------------------------------------------

   begin
not_implemented;
   end Process_Command;

   ---------------------------------------------------------------
   procedure Reload_CSS_Button_Click_Handler (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

      Windows_Connection   : constant Full_Window_Connection_Class_Access :=
                              Full_Window_Connection_Class_Access (
                                 Object.Connection_Data);
      State                : Configuration.Camera.State.State_Type'class renames
                              Standard.Camera.Configurations.
                                 Get_Read_Only_Camera_Configuration_State.all;
      Camera_CSS           : constant String := State.Get_CSS_Path;

   begin
      Ada.Text_IO.Put_Line (Quote ("reload CSS ", Camera_CSS));
--    Main_Data.GUI_Window.Document.Load_CSS ("/css/history.css");
      Windows_Connection.GUI_Window.Document.Load_CSS (Camera_CSS);
--    Object.Load_CSS (Camera_CSS);
   end Reload_CSS_Button_Click_Handler;

   ----------------------------------------------------------------
   overriding
   procedure Reset_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      Window_Connection.Update_Event.Reset_Event;
   end Reset_Update_Event;

   ---------------------------------------------------------------
   overriding
   procedure Set_Main_Created (
      Window_Connection          : in out Full_Window_Connection_Type;
      Value                      : in     Boolean) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img);
      Window_Connection.Main_Created := Value;
   end Set_Main_Created;

--   ----------------------------------------------------------------
--   procedure Run (
--      Directory                  : in     String;
--      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
--      Verbose                    : in     Boolean;
--      Wait_For_Message_Loop_Exit        : in     Boolean) is -- should be false for
--                                                      -- multiple unit tests
--   ----------------------------------------------------------------
--
--   begin
--      Log_In (Debug, Quote ("directory", Directory) &
--         " Wait_For_Message_Loop_Exit " & Wait_For_Message_Loop_Exit'img &
--         " port" & Port'img);
--
--      if Directory'length > 0 then
--         Ada.Directories.Set_Directory (Directory);
--      end if;
--      Log_Here (Debug);
--      GNOGA.Application.Open_URL;
--      Log_Here (Debug);
--      Camera.Base.Initialize_GNOGA (
--         Application_Title    => "Camera",
--         Handler              => On_Connect'Unrestricted_Access,
--         Handler_Path         => "default",
--         Port                 => Port,
----       Start_Message_Loop   => True,
--         Verbose              => Verbose,
--         Wait_For_Message_Loop_Exit  => Wait_For_Message_Loop_Exit);
--
----    if Wait_For_Message_Loop_Exit then
----       Log_Here (Debug, "wait for completion");
----       GNOGA_Ada_Lib.Base.Message_Loop_SignalWait;
----    end if;
--
--      Log_Out (Debug);
--
--   exception
--      when Fault: others =>
--         Trace_Exception (Debug, Fault);
--         raise;
--
--   end Run;
--
   ---------------------------------------------------------------
   function Running return Boolean is
   ---------------------------------------------------------------

   begin
      return Started;
   end Running;

   ---------------------------------------------------------------
   procedure Set_Mouse_Action (
      Connection_Data            : in out Window_Connection_Type;
      Action                     : in     Mouse_Click_Action_Type) is
   ---------------------------------------------------------------

   begin
      Connection_Data.Mouse_Action := Action;
   end Set_Mouse_Action;

-- ---------------------------------------------------------------
-- overriding
-- function Shared_Connection (
--    Window_Connection            : in   Full_Window_Connection_Type
-- ) return Base.Camera_State_Class_Access is
-- ---------------------------------------------------------------
--
-- begin
--    return Window_Connection.Shared_Connection;
-- end Shared_Connection;

-- ---------------------------------------------------------------
-- overriding
-- procedure Set_Main_Window (
--    Window_Connection         : in out Full_Window_Connection_Type;
--    Main_Window             : in     Gnoga.Gui.Window.
--                                        Pointer_To_Window_Class) is
-- ---------------------------------------------------------------
--
-- begin
--    Window_Connection.Main_Window := Main_Window;
-- end Set_Main_Window;

-- ---------------------------------------------------------------
-- procedure Set_Mouse_Action (
--    Window_Connection       : in out Full_Window_Connection_Type;
--    Action                  : in     Camera.Mouse_Click_Action_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Window_Connection.Mouse_Action := Action;
-- end Set_Mouse_Action;

--   ---------------------------------------------------------------
--   procedure Set_Preset (
--      Window_Connection             : in out Full_Window_Connection_Type;
--      Preset                        : in     Preset_ID_Type) is
--   ---------------------------------------------------------------
--
--   begin
--not_Implmeented;
--   end Set_Preset;

   ---------------------------------------------------------------
   procedure Trace_Button_Click_Handler (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   pragma Unreferenced (Object);
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      loop
         declare
            Response                : constant String := Ask.Ask_String ("enter trace options");
   --       Response                : Gnoga.Gui.Plugin.Message_Boxes.Message_Box_Result :=
   --                                  Gnoga.Gui.Plugin.Message_Boxes.Message_Box (
   --                                     Parent   => Object,
   --                                     Title    => "Command Line Options",
   --                                     Text     => "Yes to process options and exit" &
   --                                                 "No to process options and enter more" &
   --                                                 "Cancel at exit dialog box with saving options",
   --                                     Syle     => Gnoga.Gui.Plugin.Message_Boxes.Yes_No_Cancel_Box);


         begin
            if Response'length = 0 then
               exit;
            end if;

            declare
               Iterator                : Camera.Lib.Source_Iterator_Type;

            begin
               Iterator.Initialize (
--                Window                  => Object'unchecked_access,
                  Modifiers               => Ada_Lib.Help.Modifiers,
                  Source                  => Response,
                  Include_Options         => True,
                  Include_Non_Options     => True);

               Ada.Text_IO.New_Line;
--             Iterator.Window := Object'unchecked_access;
--             Camera.Lib.Get_Modifyable_Options.Process (Iterator);
               Log_Out (Debug);
            end;

         exception
            when Fault: others => -- Ada_Lib.Command_Line_Iterator.Not_Option =>
               Trace_Message_Exception (Debug, Fault, "Invalid options" &
                  Quote (" response", Response));
               Ada_Lib.Options.Parsing_Failed;
         end;
      end loop;
   end Trace_Button_Click_Handler;

   ---------------------------------------------------------------
   overriding
   procedure Trigger_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type) is
   ---------------------------------------------------------------
   begin
      Log_Here (Debug);
      Window_Connection.Update_Event.Set_Event;
   end Trigger_Update_Event;

   ---------------------------------------------------------------
   function Unit_Test_Suite
   return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

   begin
      return Unit_Test.Suite;
   end Unit_Test_Suite;
   ---------------------------------------------------------------
   overriding
   procedure Wait_For_Update_Event (
      Window_Connection            : in out Full_Window_Connection_Type) is
   ---------------------------------------------------------------

   begin
      Window_Connection.Update_Event.Wait_For_Event;
      Log_Here (Debug);
   end Wait_For_Update_Event;

   ---------------------------------------------------------------
   function Window_ID_Equal (
      Left, Right                : in     Window_ID_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
not_implemented;
return false;
   end Window_ID_Equal;

   ---------------------------------------------------------------
   function Window_ID_Hash (
      Key                        : in     Window_ID_Type
   ) return Ada.Containers.Hash_Type is
   ---------------------------------------------------------------

   begin
not_implemented;
return Ada.Containers.Hash_Type'first;
   end Window_ID_Hash;

begin
--Debug := True;
--Trace_Options := True;
-- Include_Task := True;
   Log_Here (Elaborate or Trace_Options or Debug);
end Camera.Main;
