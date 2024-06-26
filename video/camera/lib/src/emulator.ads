with Camera.Lib.Base;

package Emulator is

   procedure Create
   with Pre => not Has_Emulator;

   procedure Halt (
      Camera                     : in out Standard.Camera.Lib.Base.Base_Camera_Type'class)
   with Pre => Has_Emulator;

   function Has_Emulator return Boolean;

   Debug                         : Boolean := False;
   Port                          : constant := 12345;
   Trace_Socket_IO               : Boolean := False;
   URL                           : constant String := "localhost";
end Emulator;
