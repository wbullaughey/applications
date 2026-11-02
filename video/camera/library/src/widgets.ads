with Ada.Exceptions;
with Gnoga.Gui.Window;

package Widgets is

   Failed                        : exception;

   procedure Report_Exception (
      Debug                      : in     Boolean;
      Window                     : in     Gnoga.Gui.Window.Pointer_To_Window_Class;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String := "");

-- Debug                         : Boolean := False;

end Widgets;
