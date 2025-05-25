with Ada.Exceptions;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Help;
with GNOGA.Ada_Lib.Base;
with Ada_Lib.Options;
with Ada_Lib.OS;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ask;
with Base;
with Camera.Commands;
with Camera.Lib;
with Configuration.Camera.State;
with GNAT.Sockets;
with Gnoga.Ada_Lib;
with Gnoga.Application.Multi_Connect;
with Gnoga.Types.Colors;
with Camera.Lib.Base;
with Camera.Commands.PTZ_Optics;

package body Main is

-- use type Base.Connection_Data_Class_Access;
-- use type Configuration.Camera.State.State_Access;
   use type Gnoga.Types.Pointer_to_Connection_Data_Class;

   procedure Exit_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

-- procedure Exit_Mouse_Button_Click_Handler (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class;
--    Mouse_Event                : in     Gnoga.Gui.Base.Mouse_Event_Record
-- ) with Pre => Object.Connection_Data /= Null;

   --  Setup GUI for each connection.
   procedure On_Connect (
      Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Gnoga.Application.Multi_Connect.
                                             Connection_Holder_Type
   ) with Pre => GNOGA.Ada_Lib.Has_Connection_Data;

   --  Application event handlers
-- procedure On_Exit (
--    Object                     : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Open_Camera (
      Camera                     :  out Standard.Camera.Commands.Camera_Class_Access);

-- --  Setup another path in to the application for submitting results
-- --  /result, see On_Connect_Handler in body of this procedure.
-- procedure On_Result_Connect (
--    Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
--    Connection                 : access Gnoga.Application.Multi_Connect.
--                                           Connection_Holder_Type);

   procedure Reload_CSS_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

   procedure Trace_Button_Click_Handler (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class
   ) with Pre => Object.Connection_Data /= Null;

-- Temporary_Connection_Data     : Connection_Data_Access;
   -- use to pass from On_Connect to Main

   Started                       : Boolean := False;

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
--    if Connection_Data.Message_Box_Dialog = Null then
--       Log_Here (Debug, Message);
--       Log_Exception (Debug, Message);
--       raise Failed with Message;
--    else
--       Connection_Data.Message_Box_Dialog.Fire_On_Close;
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
   function Did_Exit (
      Main_Data                  : in     Main_Data_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Main_Data.Exited;
   end Did_Exit;

   ---------------------------------------------------------------
   procedure Exit_Button_Click (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Object.Connection_Data.all);
   begin
      Log_In (Debug);
--    if Connection_Data = Null then   -- GNOGA already closed
--       Log_Out (Debug);
--       return;
--    end if;

      declare
         Main_Data               : Main_Data_Type renames Connection_Data.Main_Data.all;
         View                    : View_Type renames Main_Data.View;
         Docker                  : Docker_Type renames View.Docker;
         Panel                   : Panel_Type renames Docker.Panel;
         Navigation              : Navigation_Type renames Panel.Navigation;

      begin
         Main_Data.Exited := True;
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

   ----------------------------------------------------------------
   function Get_Adjust_Card (
      Main_Data                     : in out Main_Data_Type
   ) return Widgets.Adjust.Adjust_Card_Access is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Deck.Cards.
         Adjust_Card'unchecked_access;
   end Get_Adjust_Card;

   ----------------------------------------------------------------
   function Get_Cards (
      Main_Data                     : in out Main_Data_Type
   ) return Cards_Access_Type is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Deck.Cards'unchecked_access;
   end Get_Cards;

   ----------------------------------------------------------------
   function Get_Configured_Card (
      Main_Data                     : in out Main_Data_Type
   ) return Widgets.Configured.Configured_Card_Access is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Deck.Cards.
         Configured_Card'unchecked_access;
   end Get_Configured_Card;

   ----------------------------------------------------------------
   function Get_Control_Card (
      Main_Data                     : in out Main_Data_Type
   ) return Widgets.Control.Control_Card_Access is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Deck.Cards.
         Control_Card'unchecked_access;
   end Get_Control_Card;

   ----------------------------------------------------------------
   function Get_Exit_Button (
      Main_Data                     : in out Main_Data_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Panel.Navigation.
         Exit_Button'unchecked_access;
   end Get_Exit_Button;

   ----------------------------------------------------------------
   function Get_Main_Window (
      Main_Data                     : in out Main_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
   ----------------------------------------------------------------

   begin
      return Main_Data.GUI_Window;
   end Get_Main_Window;

   ----------------------------------------------------------------
   function Get_Tabs (
      Main_Data                     : in out Main_Data_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class is
   ----------------------------------------------------------------

   begin
      return Main_Data.View.Docker.Deck.Tabs'unchecked_access;
   end Get_Tabs;

   ----------------------------------------------------------------
   function Main_Window (
      Navigation                 : in     Navigation_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return Navigation.Parent.Parent.Parent.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      Cards                      : in     Cards_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return Cards.Parent.Parent.Parent.Parent.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      Deck                       : in     Deck_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return Deck.Parent.Parent.Parent.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      Panel                      : in     Panel_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return Panel.Parent.Parent.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      Docker                     : in     Docker_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return Docker.Parent.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      View                       : in     View_Type
   ) return Gnoga.Gui.Base.Pointer_To_Base_Class is
   ----------------------------------------------------------------

   begin
      return View.Parent;
   end Main_Window;

   ----------------------------------------------------------------
   function Main_Window (
      Main_Data                  : in     Main_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
   ----------------------------------------------------------------

   begin
      return Main_Data.GUI_Window;
   end Main_Window;

   ----------------------------------------------------------------
   procedure On_Connect (
      Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Gnoga.Application.Multi_Connect.
                                             Connection_Holder_Type) is
   pragma Unreferenced (Connection);
   ----------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Standard.GNOGA.Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type
                                    renames Connection_Data.State;
   begin
      Log_In (Debug, "started " & Started'img);
      declare
         Main_Data                  : Main_Data_Type renames
                                       Connection_Data.Main_Data.all;
         View                       : View_Type renames Main_Data.View;
         Docker                     : Docker_Type renames View.Docker;
         Deck                       : Deck_Type renames Docker.Deck;
         Panel                      : Panel_Type renames Docker.Panel;
         Navigation                 : Navigation_Type renames Panel.Navigation;
         Cards                      : Cards_Type renames Deck.Cards;
         Tabs                       : Gnoga.Gui.View.Card.Tab_Type renames Deck.Tabs;
         Adjust_Card                : Widgets.Adjust.Adjust_Card_Type renames
                                       Cards.Adjust_Card;
         Configured_Card            : Widgets.Configured.Configured_Card_Type renames
                                       Cards.Configured_Card;
         Control_Card               : Widgets.Control.Control_Card_Type renames
                                       Cards.Control_Card;
         Video_Card                 : Widgets.Video.Video_Card_Type renames
                                       Cards.Video_Card;

      begin
         if not Started then
            Open_Camera (Connection_Data.Camera);
         end if;

--       if Started then
--          raise Failed with "already started";
--       end if;
         Started := True;

         Main_Window.Connection_Data (Connection_Data'unchecked_access);
         Connection_Data.Set_Main_Window (Main_Window'unchecked_access);
         View.Create (Main_Window); --, ID => "Main View");

         declare
            CSS_Path             : constant String :=
                                    State.CSS_Path.Coerce;
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

         Main_Data.GUI_Window := Gnoga.Gui.Window.Pointer_To_Window_Class'(
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
         Control_Card.Class_Name (Configuration.Camera.Control_Card_Style);

         Log_Here (Debug, " add Control_Card card");
         Cards.Add_Card (
            Name => Widgets.Control.Widget_Name,
            Card => Control_Card'access);

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
      end;

--    GNOGA.Ada_Lib.Base.Set_Base (new Main_Base_Type);
      Standard.GNOGA.Ada_Lib.Base.Set_Main_Created (True);
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
      Camera                     :  out Standard.Camera.Commands.
                                          Camera_Class_Access) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Standard.GNOGA.Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      Port_Number                : constant Ada_Lib.Socket_IO.Port_Type :=
                                    State.Get_Host_Port;
      Camera_Address             : constant Ada_Lib.Socket_IO.Address_Type :=
                                       State.Get_Host_Address;
   begin
      Log_In (Debug,
         Quote (" Camera_URL", Camera_Address.Image) &
         " port" & Port_Number'img);

      Camera := Standard.Camera.Commands.Camera_Class_Access'(
         new Standard.Camera.Commands.PTZ_Optics.PTZ_Optics_Type);

      Camera.Open (Camera_Address, Port_Number);
      Log_Out (Debug);

   exception

      when Fault: GNAT.Sockets.Host_Error =>
         Trace_Exception (Debug, Fault, Here);
         Put_Line ("Could not open camera. Error " &
            Ada.Exceptions.Exception_Message (Fault));
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);

   end Open_Camera;

   ---------------------------------------------------------------
   procedure Reload_CSS_Button_Click_Handler (
      Object            : in out Gnoga.Gui.Base.Base_Type'Class) is
   ---------------------------------------------------------------

      Connection_Data            : Base.Connection_Data_Type renames
                                    Base.Connection_Data_Type (
                                       Standard.GNOGA.Ada_Lib.Get_Connection_Data.all);
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      Camera_CSS                 : constant String := State.CSS_Path.Coerce;
      Main_Data                  : Main_Data_Type renames Connection_Data.
                                    Main_Data.all;

   begin
      Ada.Text_IO.Put_Line (Quote ("reload CSS ", Camera_CSS));
--    Main_Data.GUI_Window.Document.Load_CSS ("/css/history.css");
      Main_Data.GUI_Window.Document.Load_CSS (Camera_CSS);
--    Object.Load_CSS (Camera_CSS);
   end Reload_CSS_Button_Click_Handler;

   ----------------------------------------------------------------
   procedure Run (
      Directory                  : in     String;
      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
      Verbose                    : in     Boolean;
      Wait_For_Completion        : in     Boolean) is -- should be false for
                                                      -- multiple unit tests
   ----------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("directory", Directory) &
         " Wait_For_Completion " & Wait_For_Completion'img &
         " port" & Port'img);

      if Directory'length > 0 then
         Ada.Directories.Set_Directory (Directory);
      end if;
      GNOGA.Application.Open_URL;
      Standard.GNOGA.Ada_Lib.Base.Initialize_GNOGA (
         Application_Title    => "Camera",
         Handler              => On_Connect'Unrestricted_Access,
         Handler_Path         => "default",
         Port                 => Port,
--       Start_Message_Loop   => True,
         Verbose              => Verbose,
         Wait_For_Completion  => Wait_For_Completion);

--    if Wait_For_Completion then
--       Log_Here (Debug, "wait for completion");
--       Standard.GNOGA.Ada_Lib.Base.Message_Loop_Signal.Wait;
--    end if;

      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Run;

   ---------------------------------------------------------------
   function Running return Boolean is
   ---------------------------------------------------------------

   begin
      return Started;
   end Running;

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

begin
--Debug := True;
--Trace_Options := True;
   Include_Task := True;
   Log_Here (Elaborate or Trace_Options);
end Main;
