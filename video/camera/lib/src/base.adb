with ADA_LIB.Trace; use ADA_LIB.Trace;
--with GNOGA_Ada_Lib;
--with Main;
--with Widgets.Adjust;
--with Widgets.Configured;

package body Base is

     use type Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access;

   ---------------------------------------------------------------
   procedure Allocate_Connection_Data is
   ---------------------------------------------------------------

      Connection_Data   : constant Standard.Base.Connection_Data_Access :=
                           Allocate_Connection_Data;
      pragma Unreferenced (Connection_Data);

   begin
      Log_Here (Debug);
   end Allocate_Connection_Data;

   ---------------------------------------------------------------
   function Allocate_Connection_Data
   return Connection_Data_Access is
   ---------------------------------------------------------------

      Connection_Data            : constant Standard.Base.Connection_Data_Access :=
                                    new Standard.Base.Connection_Data_Type;
   begin
      Log_Here (Debug);
      Connection_Data.Main_Data := new Main.Main_Data_Type;
      GNOGA_Ada_Lib.Set_Connection_Data (
         GNOGA_Ada_Lib.Connection_Data_Class_Access (Connection_Data));
      return Connection_Data;
   end Allocate_Connection_Data;

   ---------------------------------------------------------------
   procedure Close_Message_Box (
      Connection_Data            : in out Connection_Data_Type) is
   ---------------------------------------------------------------

      Message                    : constant String :=
                                    "message box dooes not exist";

   begin
      Log_In (Debug);
      if Connection_Data.Message_Box_Dialog = Null then
         Log_Here (Debug, Message);
         Log_Exception (Debug, Message);
         raise Failed with Message;
      else
         Connection_Data.Message_Box_Dialog.Fire_On_Close;
      end if;
      Log_Out (Debug);
   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug, Fault);
         raise;

   end Close_Message_Box;

   ----------------------------------------------------------------
   function Get_Adjust_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Adjust.Adjust_Card_Access is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Adjust_Card;
   end Get_Adjust_Card;

   ----------------------------------------------------------------
   function Get_Cards (
      Connection_Data            : in out Connection_Data_Type
   ) return Main.Cards_Access_Type is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Cards;
   end Get_Cards;

   ----------------------------------------------------------------
   function Get_Configured_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Configured.Configured_Card_Access is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Configured_Card;
   end Get_Configured_Card;

   ----------------------------------------------------------------
   function Get_Control_Card (
      Connection_Data            : in out Connection_Data_Type
   ) return Widgets.Control.Control_Card_Access is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Control_Card;
   end Get_Control_Card;

-- ----------------------------------------------------------------
-- function Get_Default_Speed (
--    Connection_Data            : in     Connection_Data_Type
-- ) return Standard.Camera.Commands.Property_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Standard.Camera.Commands.Property_Type (
--       Connection_Data.State.Default_Speed);
-- end Get_Default_Speed;

   ----------------------------------------------------------------
   function Get_Exit_Button (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.Element.Common.Pointer_To_Button_Class is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Exit_Button;
   end Get_Exit_Button;

   ----------------------------------------------------------------
   function Get_Main_Window (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.Window.Pointer_To_Window_Class is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Main_Window;
   end Get_Main_Window;

   ----------------------------------------------------------------
   function Get_Tabs (
      Connection_Data            : in out Connection_Data_Type
   ) return Gnoga.Gui.View.Card.Pointer_To_Tab_Class is
   ----------------------------------------------------------------

   begin
      return Connection_Data.Main_Data.Get_Tabs;
   end Get_Tabs;

   ----------------------------------------------------------------
   procedure Halt is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
   end Halt;

   ----------------------------------------------------------------
   procedure Message_Box (
      Connection_Data            : in out Connection_Data_Type;
      Title, Text                : in     String;
      Style                      : in     Gnoga.Gui.Plugin.Message_Boxes.
                                             Message_Box_Type :=
                                             Gnoga.Gui.Plugin.Message_Boxes.OK_Box) is
   ----------------------------------------------------------------

      Message_Box_Dialog         : aliased Gnoga.Gui.Plugin.jQueryUI.
                                    Widget.Dialog_Type;

   begin
      Log_In (Debug, Quote ("text", Text));
      Connection_Data.Message_Box_Dialog := Message_Box_Dialog'unchecked_access;
      Connection_Data.Message_Box_Result :=
         Gnoga.Gui.Plugin.Message_Boxes.Message_Box (
            Message_Box_Dialog, -- Connection_Data.Main_Window.all,
            Title, Text, Style);
      Log_Out (Debug, "result " & Connection_Data.Message_Box_Result'img);
      Connection_Data.Message_Box_Dialog := Null;
   end Message_Box;

   ----------------------------------------------------------------
   procedure Reset_Update_Event (
      Connection_Data            : in out Connection_Data_Type) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      Connection_Data.Update_Event.Reset_Event;
   end Reset_Update_Event;

   ---------------------------------------------------------------
   procedure Trigger_Update_Event (
      Connection_Data            : in out Connection_Data_Type) is
   ---------------------------------------------------------------
   begin
      Log_Here (Debug);
      Connection_Data.Update_Event.Set_Event;
   end Trigger_Update_Event;

   ---------------------------------------------------------------
   procedure Wait_For_Update_Event (
      Connection_Data            : in out Connection_Data_Type) is
   ---------------------------------------------------------------

   begin
      Connection_Data.Update_Event.Wait_For_Event;
      Log_Here (Debug);
   end Wait_For_Update_Event;

begin
--Debug := True;
   Include_Task := True;
   Log_Here (Elaborate or Trace_Options);
end Base;
