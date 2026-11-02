with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Widgets is

      -------------------------------------------------------------
   procedure Report_Exception (
      Debug                      : in     Boolean;
      Window                     : in     Gnoga.Gui.Window.Pointer_To_Window_Class;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "") is
      -------------------------------------------------------------

   begin
not_implemented;
   end Report_Exception;

end Widgets;
