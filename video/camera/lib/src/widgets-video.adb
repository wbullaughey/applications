with ADA_LIB.Trace; use ADA_LIB.Trace;
--with GNOGA.ADA_LIB;
--with Main;

package body Widgets.Video is

   ----------------------------------------------------------------
   procedure Create (
      Video_Card                 : in out Video_Card_Type;
      Main_Window                : in out Gnoga.GUI.Window.Window_Type'Class;
      Cards                      : in out Gnoga.Gui.Base.Base_Type'Class) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Video_Card.Create (Cards, ID => "Video_Card");
      Video_Card.Place_Holder.Create (
         Parent      => Video_Card,
         Content     => "place holder",
         ID          => "place_holder");
      Log_Out (Debug);

   exception
      when Fault : others =>
         Trace_Exception (Fault, Here);
         raise;

   end Create;

end Widgets.Video;

