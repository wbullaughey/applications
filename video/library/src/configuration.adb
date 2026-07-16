with Ada.Tags;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Configuration is

   With_Camera    : Boolean := True;   -- default to have a camera

   ----------------------------------------------------------------
   function Have_Camera
   return Boolean is
   ----------------------------------------------------------------

   begin
      return With_Camera;
   end Have_Camera;

   ----------------------------------------------------------------
   procedure No_Camera is
   ----------------------------------------------------------------

   begin
      With_Camera := False;
   end No_Camera;

   package body Configuration_Package is

      ----------------------------------------------------------------
      function Is_Loaded (
         Configuration           : in     Configuration_Type;
         From                    : in     String := Ada_Lib.Trace.Here
      ) return Boolean is
      ----------------------------------------------------------------

         Result   : constant Boolean := Configuration.Loaded;

      begin
         return Log_Here (Result, Trace_Pre_Post (Result, Debug),
            "state from " & From & " " &
            "address " & Image (Configuration'address) &
            " class " & Ada.Tags.Expanded_Name (
               Configuration_Type'class (Configuration)'tag));
      end Is_Loaded;

      ----------------------------------------------------------------
      procedure Set_Loaded (
         Configuration           : in out Configuration_Type;
         Value                   : in     Boolean;
         From                    : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         Log_Here (Debug, "value " & Value'img & " class " &
            Ada.Tags.Expanded_Name (Configuration_Type'class (Configuration)'tag) &
            " address " & Image (Configuration'address) &
            " from " & From);
         Configuration.Loaded := Value;
      end Set_Loaded;

      ----------------------------------------------------------------
      procedure Unload (
         Configuration            : in out Configuration_Type) is
      ----------------------------------------------------------------

      begin
         Configuration.Loaded := False;
      end Unload;

   end Configuration_Package;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Configuration;

