with Ada_Lib.Strings;
with Ada_Lib.Timer;
with Base;

package body Events is

   ---------------------------------------------------------------------------
   procedure Initialize (
      Event                      : in out Button_Push_Event_Type;
      Connection_Data            : in     Base.Connection_Data_Class_Access;
      Wait                       : in     Duration;
      Description                : in     String := "";
      Dynamic                    : in     Boolean := False;
      Repeating                  : in     Boolean := False) is
   ---------------------------------------------------------------------------

   begin
      Event.Connection_Data := Connection_Data;
      Event.Description := Description;
      Event.Dynamic := Dynamic;
      Event.Wait := Wait;
      Event.Repeating := Repeating;
   end Initialize;


end Events;
