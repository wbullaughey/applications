--with Ada_Lib.Socket_IO;
--with Ada_Lib.Strings.Unlimited;
with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Common;
--with Gnoga.Gui.Element.Form;
--with Gnoga.Gui.Element.Table;
with Gnoga.Gui.View;
with Gnoga.GUI.Window;

package Widgets.Video is

   Failed                        : exception;

   type Video_Card_Type          is new Gnoga.Gui.View.View_Type with private;
   type Video_Card_Class_Access  is access all Video_Card_Type'class;

   procedure Create (
      Video_Card                 : in out Video_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.Base.Base_Type'Class);

   Debug                         : Boolean := False;
   Widget_Name                   : constant String := "Video";

private

   type Video_Card_Type               is new Gnoga.Gui.View.View_Type with record
      Place_Holder               : Gnoga.Gui.Element.Common.Button_Type;
   end record;

end Widgets.Video;

