----with Ada.Text_IO; use  Ada.Text_IO;
--with Ada_Lib.GNOGA;
with Ada_Lib.Parser;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with ADA_LIB.Trace; use ADA_LIB.Trace;
--with Camera.Command_Queue;
--with Camera.Lib.Base;
with Camera.Lib.Connection;
--with Camera.Commands;
--with Configuration.Camera.Setup;
--with Configuration.State; use Configuration;
--with Main;
--with Camera.Lib.Base;
--with Camera.Command_Queue;

package body Widgets.Adjust is

   use type Camera.Lib.Connection.Mouse_Click_Action_Type;

   type Response_Buffer_Type     is new Camera.Response_Buffer_Type with null record;

-- type Response_Buffer_Access   is access all Response_Buffer_Type;

   overriding
   function Callback (
      Response                  : in out Response_Buffer_Type
   ) return Camera.Status_Type;

   Column_Labels                 : aliased Column_Labels_Type := (
      new String'("Vertical Scrollbar"),
      new String'("Camera"),
      new String'("Hot Keys"));

   procedure Key_Down_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Keyboard_Event             : in     GNOGA.GUI.Base.Keyboard_Event_Record);

   procedure Mouse_Click_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Mouse_Event                : in     GNOGA.GUI.Base.Mouse_Event_Record);


   procedure Mouse_Move_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Mouse_Event                : in     GNOGA.GUI.Base.Mouse_Event_Record);

   function Parse_Mouse_Action (
      ID                         : in     String
   ) return Camera.Lib.Connection.Mouse_Click_Action_Type;

-- ----------------------------------------------------------------
-- function Action_ID (
--    Action                     : in     Base.Mouse_Click_Action_Type
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return Widget_Name & "-" & Action'img;
-- end Action_ID;

   ----------------------------------------------------------------
   overriding
   function Callback (
      Response                  : in out Response_Buffer_Type
   ) return Camera.Status_Type is
   pragma Unreferenced (Response);
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);
      return Camera.Fault;
   end Callback;

   ----------------------------------------------------------------
   procedure Create (
      Adjust_Card            : in out Adjust_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.View.View_Base_Type'Class) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Adjust_Package.Widget_Type (Adjust_Card).Create (
         Main_Window    => Main_Window,
         Name           => Widget_Name,
         Number_Columns => Outer_Column_Index_Type'last,
         Number_Rows    => Bottom_Row,
         Parent         => Cards,
         Parent_Form    => Null);
--    Adjust_Card.Class_Name (Configuration.Camera.Adjust_Card_Style);

      Adjust_Card.On_Key_Press_Handler (Key_Down_Handler'access);
      Adjust_Card.On_Mouse_Move_Handler (Mouse_Move_Handler'access);

      Log_Out (Debug, Quote ("Adjust_Card property style",
         Adjust_Card.Property ("style")) &
         Quote ("Adjust_Card style", Adjust_Card.Style ("style")));

   exception
      when Fault: others =>
         Log_Exception (True, Fault, "creating a Adjust card");
         raise;

   end Create;

   -------------------------------------------------------------------
   function Create_Labels (
      Number_Columns             : in     Outer_Column_Index_Type
   ) return Column_Labels_Access is
   pragma Unreferenced (Number_Columns);
   -------------------------------------------------------------------

   begin
      Log_Here (Debug);
      return Column_Labels'access;
   end Create_Labels;

   -------------------------------------------------------------------
   function Event_Image (
      Mouse_Event                : in     GNOGA.GUI.Base.Mouse_Event_Record
   ) return String is
   -------------------------------------------------------------------

   begin
      return
         Quote (" message " & Mouse_Event.Message'img) &
         " X " & Mouse_Event.X'img &
         " Y " & Mouse_Event.Y'img &
         " screen X " & Mouse_Event.Screen_X'img &
         " screen Y " & Mouse_Event.Screen_Y'img &
         " left button " & Mouse_Event.Left_Button'img &
         " middle button " & Mouse_Event.Middle_Button'img &
         " right button " & Mouse_Event.Right_Button'img &
         " Alt " & Mouse_Event.Alt'img &
         " Control " & Mouse_Event.Control'img &
         " Shift " & Mouse_Event.Shift'img &
         " Meta " & Mouse_Event.Meta'img;
   end Event_Image;

   -------------------------------------------------------------------
   function Get_Cell (
      Adjust_Card                : in     Adjust_Card_Type;
      Column_Index               : in     Outer_Column_Index_Type;
      Row_Index                  : in     Outer_Row_Index_Type
   ) return Cell_Class_Access is
   -------------------------------------------------------------------

   begin
      Log_Here (Debug, "adjust card class " & Tag_Name (
         Adjust_Card_Type'class (Adjust_Card)'tag));

      declare
         Row                     : constant Adjust_Package.Row_Class_Access :=
                                    Adjust_Card.Get_Row (Row_Index);
         Column                  : constant Outer_Package.Outer_Column_Access :=
                                    Outer_Package.Outer_Column_Access (
                                       Row.Get_Column (Column_Index));
      begin
         return Column.Cell;
      end;
   end Get_Cell;

   -------------------------------------------------------------------
   procedure Key_Down_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Keyboard_Event             : in     GNOGA.GUI.Base.Keyboard_Event_Record) is
   pragma Unreferenced (Object);
   -------------------------------------------------------------------

   begin
      Log_Here (Debug, Quote ("key", Character'val (Keyboard_Event.Key_Code)) &
         " alt " & Keyboard_Event.Alt'img &
         " control " & Keyboard_Event.Control'img &
         " shift " & Keyboard_Event.Shift'img &
         " meta " & Keyboard_Event.Meta'img);
   end Key_Down_Handler;

   -------------------------------------------------------------------
   procedure Mouse_Click_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Mouse_Event                : in     GNOGA.GUI.Base.Mouse_Event_Record) is
   -------------------------------------------------------------------

      Connection_Data         : Camera.Lib.Connection.Connection_Data_Type renames
                                 Camera.Lib.Connection.Connection_Data_Type (
                                    Object.Connection_Data.all);
      Mouse_Action            : constant Camera.Lib.Connection.Mouse_Click_Action_Type :=
                                 Parse_Mouse_Action (Object.ID);
   begin
      Log_In (Debug, Quote ("object id", Object.ID) &
         Event_Image (Mouse_Event));

      if Mouse_Action /= Camera.Lib.Connection.No_Change then
         Connection_Data.Mouse_Action := Mouse_Action;
      end if;
      Log_Out (Debug);
   end Mouse_Click_Handler;

   -------------------------------------------------------------------
   procedure Mouse_Move_Handler (
      Object                     : in out GNOGA.GUI.Base.Base_Type'Class;
      Mouse_Event                : in     GNOGA.GUI.Base.Mouse_Event_Record) is
   -------------------------------------------------------------------

--    Response_Buffer         : Response_Buffer_Access := new
--                               Response_Buffer_Type;
--    Connection_Data         : Camera.Lib.Connection.Connection_Data_Type renames
--                               Camera.Lib.Connection.Connection_Data_Type (
--                                  Object.Connection_Data.all);
      Mouse_Action            : constant Camera.Lib.Connection.Mouse_Click_Action_Type :=
                                 Parse_Mouse_Action (Object.ID);


   begin
      Log_In (Debug, Quote ("object id", Object.ID) &
         " action " & Mouse_Action'img &
         Event_Image (Mouse_Event));

--    case Mouse_Action is
--
--       when Camera.Lib.Connection.Any_Scroll =>
--          Connection_Data.Camera.Asynchronous (
--             Camera.Position_Relative,
--          Camera.Command_Queue.Relative_Command (
--             Connection_Data.Camera.all,
--             Camera.Commands.Relative_Type (Mouse_Event.X),
--             Camera.Commands.Relative_Type (Mouse_Event.Y),
--             Connection_Data.Camera_Pan_Speed,
--             Connection_Data.Camera_Tilt_Speed);
--
--       when Camera.Lib.Connection.Horizontal_Scroll =>
--          Camera.Command_Queue.Relative_Command (
--             Connection_Data.Camera.all,
--             Camera.Commands.Relative_Type (Mouse_Event.X), 0,
--             Connection_Data.Camera_Pan_Speed, 0);
--
--       when Camera.Lib.Connection.No_Action | Camera.Lib.Connection.No_Change =>
--          Log_Out (Debug);
--          return;
--
--       when Camera.Lib.Connection.Vertical_Scroll =>
--          Camera.Command_Queue.Relative_Command (
--             Connection_Data.Camera.all, 0,
--             Camera.Commands.Relative_Type (Mouse_Event.Y), 0,
--             Connection_Data.Camera_Tilt_Speed);
--
--    end case;

--    Connection_Data.Camera.Set_Absolute (Connection_Data.Camera_Pan,
--       Connection_Data.Camera_Tilt);
      Log_Out (Debug);
   end Mouse_Move_Handler;

   -------------------------------------------------------------------
   function Parse_Mouse_Action (
      ID                         : in     String
   ) return Camera.Lib.Connection.Mouse_Click_Action_Type is
   -------------------------------------------------------------------

      use Ada_Lib.Parser;

   begin
      Log_In (Debug, Quote ("ID", ID));
      declare
         Iterator                : Iterator_Type := Initialize (
                                    Value          => ID,
                                    Seperators     => "-",
                                    Ignore_Multiple_Seperators
                                                   => False,
                                    Comment_Seperator
                                                   => No_Seperator,
                                    Trim_Spaces    => False,
                                    Quotes         => "");
         Action_Table            : constant array (Outer_Column_Index_Type,
                                    Outer_Row_Index_Type) of
                                       Camera.Lib.Connection.Mouse_Click_Action_Type := (
                                       Left_Column    => (
                                          Top_Row     => Camera.Lib.Connection.No_Action,
                                          Center_Row  => Camera.Lib.Connection.Vertical_Scroll,
                                          Bottom_Row  => Camera.Lib.Connection.No_Action),
                                       Center_Column  => (
                                          Top_Row     => Camera.Lib.Connection.Horizontal_Scroll,
                                          Center_Row  => Camera.Lib.Connection.Any_Scroll,
                                          Bottom_Row  => Camera.Lib.Connection.Horizontal_Scroll),
                                       Right_Column  => (
                                          Top_Row     => Camera.Lib.Connection.No_Action,
                                          Center_Row  => Camera.Lib.Connection.Vertical_Scroll,
                                          Bottom_Row  => Camera.Lib.Connection.No_Action));
      begin
         Iterator.Next;    -- skip prefix

         if not Iterator.At_End then

            declare
               Row               : constant Outer_Row_Index_Type :=
                                    Outer_Row_Index_Type'Value (
                                       Iterator.Get_Value (Do_Next => True));
            begin
               if not Iterator.At_End then
                  declare
                     Column      : constant Outer_Column_Index_Type :=
                                    Outer_Column_Index_Type'Value (
                                       Iterator.Get_Value (Do_Next => True));
                  begin
                     if not Iterator.At_End then
                        declare
--                         Kind  : constant String :=
--                                  Iterator.Get_Value (Do_Next => False);
                           Result: constant Camera.Lib.Connection.Mouse_Click_Action_Type :=
                                    Action_Table (Column, Row);
                        begin
                           Log_Out (Debug, Result'img);
                           return Result;
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
      end;

      Log_Out (Debug, "in active window");
      return Camera.Lib.Connection.No_Change;

   exception
      when Fault: others =>
         Trace_Message_Exception (Fault, Quote ("id", ID));
         raise;
   end Parse_Mouse_Action;

   -------------------------------------------------------------------
   overriding
   procedure Verify_Widget (
      Widget                     : in     Adjust_Card_Type;
      Verify_Parameter           : in     Adjust_Package.
                                             Verify_Parameter_Class_Access) is
   -------------------------------------------------------------------

   begin
      Not_Implemented;
   end Verify_Widget;

package body Outer_Package is

--    procedure Refresh_Row (
--       Row                        : in     Gnoga.Gui.Base.Pointer_To_Base_Class);

--    procedure Update_Handler (
--       Object                  : in out Gnoga.Gui.Base.Base_Type'Class);

      ----------------------------------------------------------------
      procedure Allocate_Column (
         Column                  : in out Generic_Cell_Package.
                                             Generic_Column_Class_Access;
         Column_Index            : in     Outer_Column_Index_Type;
         Table_Row               : in     Outer_Row_Index_Type) is
      ----------------------------------------------------------------

         Local_Column            : constant Outer_Column_Access :=
                                    new Outer_Column_Type;
      begin
         Log_Here (Debug, "column " & Column_Index'img &
            " row " & Table_Row'img);
         Column := Generic_Cell_Package.Generic_Column_Class_Access (Local_Column);
         Local_Column.Cell := new Cell_Type (Column_Index);
      end Allocate_Column;

      ----------------------------------------------------------------
--    overriding
      procedure Create (
         Element                 : in out Gnoga.Gui.Element.Form.Text_Type;
         Form                    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Value                   : in     String := "";
         Name                    : in     String := "";
         ID                      : in     String := "") is
      ----------------------------------------------------------------

      begin
         Log_In (Debug, Quote ("value", Value) & Quote (" name", Name) &
            Quote (" id", ID));
         Gnoga.Gui.Element.Form.Text_Type (Element).Create (
            Form     => Form,
            Id       => Id,
            Name     => Name,
            Size     => 20,
            Value    => Value);
         Log_Out (Debug);
      end Create;
--
      ----------------------------------------------------------------
      overriding
      procedure Create (
         Meter                   : in out Slider_Type;
         Parent                  : in out Gnoga.Gui.Base.Base_Type'Class;
         Value                   : in     Integer := 0;
         High                    : in     Integer := 100;
         Low                     : in     Integer := 0;
         Maximum                 : in     Integer := 100;
         Minimum                 : in     Integer := 0;
         Optimum                 : in     Integer := 50;
         ID                      : in     String := "") is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Gnoga.Gui.Element.Common.Meter_Type (Meter).Create (
            Parent, Value, High, Low, Maximum, Minimum, Optimum, ID);
         Log_Out (Debug);
      end Create;

        ----------------------------------------------------------------
        overriding
        procedure Create_Cell (
           Cell                    : in out Cell_Type;
           Form                    : in     Gnoga.Gui.Element.Form.
                                               Pointer_To_Botton_Class;
           Row                     : in out Gnoga.Gui.Element.Table.
                                               Table_Row_Type'class;
           Column                  : in out Generic_Cell_Package.
                                               Generic_Column_Type'class;
           Table_Column            : in     Outer_Column_Index_Type;
           Table_Row               : in     Outer_Row_Index_Type) is
        ----------------------------------------------------------------

--         Connection_Data         : Camera.Lib.Connection.Connection_Data_Type renames
--                                    Camera.Lib.Connection.Connection_Data_Type (
--                                       Form.Connection_Data.all);
--         Configuration_ID        : Outer_Row_Index_Type renames Table_Row;
           Name                    : constant String := Row.ID;
           Center_Box_ID           : constant String := Name & "-" &
                                      Ada_Lib.Strings.Trim (Table_Column'img &
                                      "-Box");
           Cell_ID                 : constant String := Name & "-" &
                                      Ada_Lib.Strings.Trim (Table_Column'img &
                                      "-Cell");
--         Field_ID               : constant String := Name & "_Field_" &
--                                    Ada_Lib.Strings.Trim (Table_Row'img) & "_" &
--                                    Ada_Lib.Strings.Trim (Table_Column'img);
--         Outer_ID               : constant Configuration.Camera.Outer_ID_Type :=
--                                    Configuration.Camera.Camera.Setup.Get_Outer_ID (
--                                       Configuration_ID);
--         Has_Preset              : constant Boolean :=
--                                    Configuration.Camera.Camera.Setup.Has_Preset (Outer_Id);
--         Preset                  : constant Configuration.Camera.Camera.Setup.Outer_Type :=
--                                    (if Has_Preset then
--                                       Configuration.Camera.Camera.Setup.Get_Preset (Outer_ID)
--                                    else
--                                        Configuration.Camera.Camera.Setup.Null_Preset);
         begin
            Log_In (Debug, "row " & Table_Row'img &
               " Column " & Table_Column'img &
               Quote (" row id" & Name) &
               Quote (" cell ID", Cell_ID) &
               Quote (" column ID", Column.ID));

            Cell.Create (Column, ID => Cell_ID);
            Cell.On_Key_Press_Handler (Key_Down_Handler'access);
            Cell.On_Mouse_Move_Handler (Mouse_Move_Handler'access);

            case Table_Column is

               when Center_Column =>
                  case Table_Row is

                     when Top_Row =>   -- slider only on top row
                        Cell.Horizontal_Slider.Create (Cell,
                           Value => 50,
                           ID    => Cell_ID & "-Horizontal_Slider");
                        Cell.Horizontal_Slider.Class_Name (
                           Configuration.Camera.Horizontal_Slider_Style);
                        Cell.Horizontal_Slider.On_Mouse_Move_Handler (
                           Mouse_Move_Handler'access);

                     when Center_Row =>
                        Cell.Center_Box.Create (Cell, ID => Center_Box_ID);
                        Cell.Center_Box.On_Mouse_Move_Handler (
                           Mouse_Move_Handler'access);
                        Cell.Center_Box.Class_Name (Configuration.Camera.Center_Box_Style);

                     when Bottom_Row =>
                        Null;
                  end case;

              when Left_Column =>
                  case Table_Row is

                     when Top_Row =>   -- slider only on top row
                        null;

                     when Center_Row =>
                        Cell.Vertical_Slider_Blank.Create (Cell, Content=>"blank");
                        Cell.Vertical_Slider_Blank.On_Mouse_Move_Handler (
                           Mouse_Move_Handler'access);
                        Cell.Vertical_Slider_Blank.Class_Name (
                           Configuration.Camera.Vertical_Slider_Blank_Style);
                        Cell.Vertical_Slider_Solid.Create (Cell, Content=>"solid");
                        Cell.Vertical_Slider_Solid.Class_Name (
                           Configuration.Camera.Vertical_Slider_Solid_Style);

                     when Bottom_Row =>
                        Null;

                  end case;

              when Right_Column =>
                  null;
           end case;

           Log_Out (Debug);
        end Create_Cell;

      ----------------------------------------------------------------
      overriding
      procedure Create_Column (
         Column                     : in out Outer_Column_Type;
         Row                        : in out Gnoga.Gui.Element.Table.
                                                Table_Row_Type'class;
         Number_Rows                : in     Outer_Row_Index_Type;
         Row_Index                  : in     Outer_Row_Index_Type;
         Column_Index               : in     Outer_Column_Index_Type;
         ID                         : in     String) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug, "number rows " & Number_Rows'img &
            " row " & Row_Index'img &
            " column " & Column_Index'img & " id " & ID);

         Gnoga.Gui.Element.Table.Table_Column_Type (Column).Create (
            Column_Span    => 1,
            Content        => "",
            ID             => ID,
            Row            => Row,
            Row_Span       => 1);

         Column.On_Mouse_Click_Handler (Mouse_Click_Handler'access);
         Log_Out (Debug);
      end Create_Column;

      ----------------------------------------------------------------
      overriding
      function Create_Column (
         Widget                  : in out Widget_Type;
         Row                     : in     Outer_Row_Index_Type;
         Column                  : in     Outer_Column_Index_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Column;

      ----------------------------------------------------------------
      overriding
      function Create_Row (
         Widget                  : in out Widget_Type;
         Row                     : in     Outer_Row_Index_Type
      ) return Boolean is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
not_implemented;
         return Log_Out (True, Debug);
      end Create_Row;

      ----------------------------------------------------------------
      overriding
      function Get_Cell (
         Column                  : in out Outer_Column_Type
      ) return Generic_Cell_Package.Cell_Class_Access is
      ----------------------------------------------------------------

      begin
         return Generic_Cell_Package.Cell_Class_Access'(
            Generic_Cell_Package.Cell_Class_Access (Column.Cell));
      end Get_Cell;

      ----------------------------------------------------------------
      procedure On_Submit (
         Object                  : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
         Log_Out (Debug);
      end On_Submit;

--    ----------------------------------------------------------------
--    procedure Refresh_Row (
--       Row                        : in     Gnoga.Gui.Base.Pointer_To_Base_Class) is
--    ----------------------------------------------------------------
--
--    begin
--       Log_In (Debug);
--       Log_Out (Debug);
--    end Refresh_Row;

      ----------------------------------------------------------------
      overriding
      procedure Update_Cell (
         Cell                    : in out Cell_Type;
         Update_Parameter        : in     Generic_Cell_Package.
                                             Update_Parameter_Type'class) is
      ----------------------------------------------------------------

      begin
         Log_In (Debug);
--       Cell.Outer_ID := Outer_ID;
--       Cell.Outer_ID_Field.Value (Outer_ID'img);
         Log_Out (Debug);
      end Update_Cell;

   end Outer_Package;

begin
--debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Widgets.Adjust;

