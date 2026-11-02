with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Video.Lib;

package body Video is

   Debug                : Boolean renames Video.Lib.Video_Options.
                           Video_Debug;

   ----------------------------------------------------------------
   procedure Dump (
      Video                      : in     Video_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("Loaded " & Video.Loaded'img);
      Put_Line ("Modified " & Video.Modified'img);
   end Dump;


   ----------------------------------------------------------------
   function Is_Loaded (
      Video                   : in     Video_Type;
      From                    : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Pre_Post (Video.Loaded, Debug,
         "Video from " & From & " " &
         "address " & Image (Video'address) &
         " class " & Ada.Tags.Expanded_Name (
            Video_Type'class (Video)'tag));
   end Is_Loaded;

   ----------------------------------------------------------------
   procedure Set_Loaded (
      Video                   : in out Video_Type;
      Value                   : in     Boolean;
      From                    : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "value " & Value'img & " class " &
         Ada.Tags.Expanded_Name (
            Video_Type'class (Video)'tag) &
         " address " & Image (Video'address) &
         " from " & From);
      Video.Loaded := Value;
   end Set_Loaded;

   ----------------------------------------------------------------
   procedure Unload (
      Video                   : in out Video_Type) is
   ----------------------------------------------------------------

   begin
      Video.Loaded := False;
   end Unload;

end Video;