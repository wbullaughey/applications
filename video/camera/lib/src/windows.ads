with Gnoga.Types;

package Windows is

   type Connection_Data_Type     is new Gnoga.Types.Connection_Data_Type with private;

   Debug                         : Boolean := False;

private

   type Connection_Data_Type     is new Gnoga.Types.Connection_Data_Type with null record;
-- end record;


end Windows;
