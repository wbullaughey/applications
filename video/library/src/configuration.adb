--with Ada_Lib.Configuration;
--with Ada_Lib.Socket_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Video.Lib;

package body Configuration is

   Debug                         : Boolean renames Video.Lib.Video_Options.Configuration_Debug;

   ---------------------------------------------------------------
   function Is_Loaded (
      Configuration           : in     Abstract_Configuration_Type;
      From                    : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Log_Here (Configuration.Loaded, Debug, "From " & From);
   end Is_Loaded;

   ---------------------------------------------------------------
   procedure Set_Loaded (
      Configuration           : in out Abstract_Configuration_Type;
      Value                   : in     Boolean;
      From                    : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img & "From " & From);
      Configuration.Loaded := Value;
   end Set_Loaded;

end Configuration;
