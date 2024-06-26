with Ada_Lib.Strings;
with Ada_Lib.Timer;
with Base;

package Events is

   type Button_Push_Event_Type   is abstract new Ada_Lib.Timer.Event_Type
                                    with private;

   procedure Initialize (
      Event                      : in out Button_Push_Event_Type;
      Connection_Data            : in     Base.Connection_Data_Class_Access;
      Wait                       : in     Duration;
      Description                : in     String := "";
      Dynamic                    : in     Boolean := False;
      Repeating                  : in     Boolean := False
   ) with Pre => Connection_Data /= Null and
                 Wait > 0.0;

private

   type Button_Push_Event_Type   is abstract new Ada_Lib.Timer.Event_Type
                                    with record
      Connection_Data            : Base.Connection_Data_Class_Access := Null;
   end record;

end Events;
