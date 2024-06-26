with ADA_LIB.Trace; use ADA_LIB.Trace;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Window;

package body Windows.Top is

   type Widget_Type is new Gnoga.Gui.View.View_Type with record
         Widget_Form : Gnoga.Gui.Element.Form.Form_Type;
         Name_Input  : Gnoga.Gui.Element.Form.Text_Type;
         Message     : Gnoga.Gui.Element.Form.Text_Area_Type;
         My_Submit   : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

   overriding
   procedure Create  (View    : in out Widget_Type;
                      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
                      ID      : in     String  := "");

   type Application_Data_Type is new Gnoga.Types.Connection_Data_Type with
      record
         Window                  : Gnoga.Gui.Window.Pointer_To_Window_Class;

         Docker                  : Gnoga.Gui.View.Docker.Docker_View_Type;
         --  Main view, will dock a view on top (Panel) for the
         --  Exit_Application button and another taking up the rest of the window
         --  with another Docker (Deck)

         Panel                   : aliased Gnoga.Gui.View.View_Type;
         Deck                    : aliased Gnoga.Gui.View.Docker.Docker_View_Type;
         --  Deck will dock a set of tabs at the top and the
         --  remainder will a card view Cards

         Tabs                    : aliased Gnoga.Gui.View.Card.Tab_Type;
         Cards                   : aliased Gnoga.Gui.View.Card.Card_View_Type;

         Widget                  : aliased Widget_Type;
--       Results                 : aliased Gnoga.Gui.View.Console.Console_View_Type;
         --  These three views will be placed in to cards in Cards
      end record;

   type Application_Data_Access is access all Application_Data_Type;

   procedure Result_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure Exit_Application (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class);

      -------------------------------------------------------------------------
   overriding
   procedure Create (
      View                       : in out Widget_Type;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      ID                         : in     String  := "") is
      -------------------------------------------------------------------------


   begin
      Log_In (Debug);
--    Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
--
--    View.Widget_Form.Create (View);
--
--    Layout_Table.Dynamic;
--    Layout_Table.Create (View);
--
--    declare
--       row  : constant Table_Row_Access := new Table_Row_Type;
--       col1 : constant Table_Column_Access := new Table_Column_Type;
--       col2 : constant Table_Column_Access := new Table_Column_Type;
--    begin
--       row.Dynamic;
--       col1.Dynamic;
--       col2.Dynamic;
--
--       row.Create (Layout_Table.all);
--       col1.Create (row.all, "Name");
--       col2.Create (row.all);
--       View.Name_Input.Create (Form  => View.Widget_Form,
--                               Size  => 40,
--                               Name  => "Name");
--       View.Name_Input.Required;
--       View.Name_Input.Place_Holder ("(Only letters and spaces permitted)");
--       View.Name_Input.Pattern ("[a-zA-Z\ ]+");
--
--       View.Name_Input.Place_Inside_Top_Of (col2.all);
--    end;
--
--    declare
--       row  : constant Table_Row_Access := new Table_Row_Type;
--       col1 : constant Table_Column_Access := new Table_Column_Type;
--       col2 : constant Table_Column_Access := new Table_Column_Type;
--    begin
--       row.Dynamic;
--       col1.Dynamic;
--       col2.Dynamic;
--
--       row.Create (Layout_Table.all);
--       row.Style ("vertical-align", "top");
--       col1.Create (row.all, "Message");
--       col2.Create (row.all);
--       View.Message.Create (Form    => View.Widget_Form,
--                            Columns => 20,
--                            Rows    => 10,
--                            Name    => "Message");
--       View.Message.Place_Inside_Top_Of (col2.all);
--    end;
--
--    View.My_Submit.Create (Form => View.Widget_Form, Value => "Submit");
--    View.My_Submit.Place_After (Layout_Table.all);
      Log_Out (Debug);
   end Create;

   ----------------------------------------------------------------------------
   procedure Connect (
      Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Gnoga.Application.Multi_Connect.
                                    Connection_Holder_Type) is
      pragma Unreferenced (Connection);
   ----------------------------------------------------------------------------

      Application_Data           : Application_Data_Access :=
                                    new Application_Data_Type;

   begin
      Main_Window.Connection_Data (Application_Data);
      Application_Data.Window := Main_Window'Unchecked_Access;

      Application_Data.Docker.Create (Main_Window);

      Application_Data.Panel.Create (Application_Data.Docker);
      Application_Data.Panel.Background_Color ("silver");

      Gnoga.Gui.Element.Common.Button_Access (
         Application_Data.Panel.New_Element
           ("Exit_Application", new Gnoga.Gui.Element.Common.Button_Type)
      ).Create (Application_Data.Panel, "Exit_Application Application");
      --  Views have built in an Element Map that can be used for storing
      --  dynamic elements. When New_Element is used it marks the stored
      --  element as Dynamic for later garbage collection.

      Application_Data.Panel.Element (
         Name     => "Exit_Application"
      ).On_Click_Handler (Exit_Application'Unrestricted_Access);
      --  Once a dynamic element is stored in a view's map it can be easily
      --  accessed using the Element property.

      Application_Data.Docker.Top_Dock (Application_Data.Panel'Access);

      Application_Data.Deck.Create (Application_Data.Docker);
      Application_Data.Docker.Fill_Dock (Application_Data.Deck'Access);

      Application_Data.Cards.Create (Application_Data.Deck);
      Application_Data.Cards.Border;
      Application_Data.Deck.Fill_Dock (Application_Data.Cards'Access);

      Application_Data.Tabs.Create (Parent    => Application_Data.Deck,
                          Card_View => Application_Data.Cards);

      Application_Data.Widget.Create (Application_Data.Cards);
      Application_Data.Widget.Widget_Form.Action ("/result");
      Application_Data.Cards.Add_Card (Name => "Widget",
                             Card => Application_Data.Widget'Access);
      Application_Data.Tabs.Add_Tab ("Widget", "Static Form", Selected => True);


--    Application_Data.Results.Create (Application_Data.Cards);
--    Application_Data.Cards.Add_Card (Name => "Results",
--                           Card => Application_Data.Results'Access);
--    Application_Data.Tabs.Add_Tab ("Results", "Interactive Results");

      Application_Data.Deck.Top_Dock (Application_Data.Tabs'Access);
      --  We wait to dock Tabs until after we have added the tabs
      --  this ensures that Tabs's Height with content is known.
   end Connect;

   ----------------------------------------------------------------------------
   procedure Exit_Application (
      Object                     : in out Gnoga.Gui.Base.Base_Type'Class) is
   ----------------------------------------------------------------------------

      Application_Data           : constant Application_Data_Access :=
                                    Application_Data_Access (Object.Connection_Data);

   begin
      Gnoga.Gui.Element.Common.Button_Access (
         Application_Data.Panel.Element ("Exit_Application")).Disabled;
      --  We can upcast to a Button_Type if Element ("Exit_Application") was not a
      --  Button_Type then we would receive a runtime exception.

      Gnoga.Application.Multi_Connect.End_Application;
   end Exit_Application;

   ----------------------------------------------------------------------------
   procedure Result_Connect (
      Main_Window                : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection                 : access Gnoga.Application.Multi_Connect.
                                    Connection_Holder_Type) is
      pragma Unreferenced (Connection, Main_Window);
   ----------------------------------------------------------------------------

--    Result_View : constant Gnoga.Gui.View.View_Access := new Gnoga.Gui.View.View_Type;

   begin
      Log_In (Debug);
--    Result_View.Dynamic;
--    Result_View.Create (Main_Window);
--
--    Result_View.Put_Line ("Name : " & Main_Window.Form_Parameter ("Name"));
--    Result_View.Put_Line ("Message : " &
--                            Main_Window.Form_Parameter ("Message"));
      Log_Out (Debug);
   end Result_Connect;

   ----------------------------------------------------------------------------
   procedure Start is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Gnoga.Application.Title ("Camera Control");

      Gnoga.Application.HTML_On_Close ("Close Camera Control");

      Gnoga.Application.Multi_Connect.Initialize;

      Gnoga.Application.Multi_Connect.On_Connect_Handler (
         Event => Connect'Unrestricted_Access,
         Path  => "default");

      Gnoga.Application.Multi_Connect.On_Connect_Handler (
         Event => Result_Connect'Unrestricted_Access,
         Path  => "result");

      Gnoga.Application.Multi_Connect.Message_Loop;
      Log_Out (Debug);
   end Start;

end Windows.Top;
