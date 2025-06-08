with Ada.Tags;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Configuration is

   ----------------------------------------------------------------
   function Is_Loaded (
      State                      : in     Root_State_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Loaded, Debug,
         "class " & Ada.Tags.Expanded_Name (Root_State_Type'class (State)'tag));
   end Is_Loaded;

   ----------------------------------------------------------------
   function Is_Loaded (
      Setup                      : in     Root_Setup_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Setup.Loaded, Debug,
         "class " & Ada.Tags.Expanded_Name (Root_Setup_Type'class (Setup)'tag));
   end Is_Loaded;

   ----------------------------------------------------------------
   procedure Set_Loaded (
      State                      :    out Root_State_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img & " from " & From);
      State.Loaded := Value;
   end Set_Loaded;

   ----------------------------------------------------------------
   procedure Set_Loaded (
      Setup                      :    out Root_Setup_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img & " from " & From);
      Setup.Loaded := Value;
   end Set_Loaded;

begin
Debug := True;
   Log_Here (Debug or Elaborate);
end Configuration;

