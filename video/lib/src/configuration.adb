with Ada.Tags;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Configuration is

   ----------------------------------------------------------------
   function Is_Loaded (
      State                      : in     Root_State_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Loaded, Debug or Trace_Pre_Post_Conditions,
         "from " & From & " " &
         "address " & Image (State'address) &
         " class " & Ada.Tags.Expanded_Name (Root_State_Type'class (State)'tag));
   end Is_Loaded;

   ----------------------------------------------------------------
   function Is_Loaded (
      Setup                      : in     Root_Setup_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Setup.Loaded, Debug or Trace_Pre_Post_Conditions,
         "from " & From & " " &
         "address " & Image (Setup'address) &
         " class " & Ada.Tags.Expanded_Name (Root_Setup_Type'class (Setup)'tag));
   end Is_Loaded;

   ----------------------------------------------------------------
   procedure Set_Loaded (
      State                      :    out Root_State_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img & " class " &
         Ada.Tags.Expanded_Name (Root_State_Type'class (State)'tag) &
         " address " & Image (State'address) &
         " from " & From);
      State.Loaded := Value;
   end Set_Loaded;

   ----------------------------------------------------------------
   procedure Set_Loaded (
      Setup                      :    out Root_Setup_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img &
         Ada.Tags.Expanded_Name (Root_Setup_Type'class (Setup)'tag) &
         " address " & Image (Setup'address) &
         " from " & From);
      Setup.Loaded := Value;
   end Set_Loaded;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Configuration;

