--with Ada_Lib.Socket_IO;
--with Configuration.Camera; use Configuration.Camera;
with Gnoga.Gui.Base;
--with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;

------------------------------------------------------------------------------
-- Control Card creates a grid of images from the camera
-- configured in state.cfg to have number rows and columns
-- for each row
--    column 0 is the row number
--    columns 1 .. <number columns> has:
--       camera image, preset, preset text, image path
------------------------------------------------------------------------------

package Widgets.Control is

   Failed                        : exception;

   type Control_Card_Type        is abstract tagged limited null record;
   type Control_Card_Access      is access all Control_Card_Type;
   type Control_Card_Class_Access
                                 is access all Control_Card_Type'class;

   procedure Class_Name (
      Card                       : in     Control_Card_Type);

   procedure Create (
      Control_Card               : in out Control_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Parent                     : in out Gnoga.Gui.Base.Base_Type'Class;
      Content                    : in     String := "";
      ID                         : in     String := "");

   function Get_Card (
      Card                       : in     Control_Card_Type
   ) return Gnoga.Gui.View.Pointer_To_View_Base_Class is abstract;

-- procedure Verify_Widget (
--    Widget                     : in     Control_Card_Type;
--    Verify_Parameter           : in     Widgets.Generic_Table.
--                                           Verify_Parameter_Class_Access);

   function Allocate_Control_Card
   return Control_Card_Class_Access;

   Debug                         : aliased Boolean := False;
   Widget_Name                   : constant String := "Control_Card";

end Widgets.Control;

