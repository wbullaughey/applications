with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
with ADA_LIB.OS;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Hex_IO;

package body Video.Lib is

   use type Ada_Lib.Options.Flag_List_Type;
-- use type Index_Type;

   Cameras     : array (Camera_ID_Type) of Camera_Class_Access :=
                  (others => Null);
   Debug_Option: constant Character := 'V';
   Debug       : Boolean renames Video_Options.Library_Debug;
   Directory_Option
               : constant Character := 'q';
   Options_With_Parameters
               : aliased constant
                  Ada_Lib.Options.Flag_List_Type :=
                     Ada_Lib.Options.Initialize (
                        Directory_Option & 'V', Ada_Lib.Options.Unmodified_Flag);
   Options_Without_Parameters
               : aliased constant
                  Ada_Lib.Options.Flag_List_Type :=
                     Ada_Lib.Options.Initialize (
                        "rS", Ada_Lib.Options.Unmodified_Flag) &
                     Ada_Lib.Options.Initialize (
                        "N", Ada_Lib.Help.Modifier);
   -------------------------------------------------------------------------
   function Camera_Configured
   return Boolean is
   -------------------------------------------------------------------------

      Video_Lib_Options
                  : constant Video_Lib_Nested_Options_Constant_Class_Access :=
                     Get_Video_Lib_Constant_Nested_Options;
      Result      : constant Boolean := Video_Lib_Options.Location /= Video.Lib.No_Location;
      Trace_Log   : constant Boolean := Trace_Pre_Post (Result, Debug);

   begin
      Log_In (Trace_Log);
      Tag_History (Trace_Log, "Nested_Options", Video_Lib_Options.all'tag);

      return Log_Out (Result, Trace_Log);

   exception
      when Fault: others =>
         Trace_Exception (Trace_Log, Fault);
         raise;

   end Camera_Configured;

   ---------------------------------------------------------------
   procedure Dump (
      Description                : in     String;
      Buffer                     : in     Buffer_Type;
      Length                     : in     Natural;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

   begin
      Dump (Buffer (Buffer'first .. Buffer'first + Index_Type (Length) - 1), Description, From);
   end Dump;

   ----------------------------------------------------------------
   procedure Dump (
      Object                  : in     Object_Type;
      What                    : in     String := "";
      From                    : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("id" & Object.Video_ID'img);
   end Dump;

--   -------------------------------------------------------------------------
--   function Get_Brand (
--      Camera         : in     Camera_Type
--   ) return Brand_Tag_Type is
--   -------------------------------------------------------------------------
--
--   begin
--not_implemented;
--return No_Brand;
--   end Get_Brand;

   -------------------------------------------------------------------------
   function Get_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Camera_Class_Access is
   -------------------------------------------------------------------------

   begin
      return Cameras (Camera_ID);
   end Get_Camera;

   ----------------------------------------------------------------
   function Get_Video_ID (
      Object  :     Object_Type
   ) return Video_ID_Type is
   ----------------------------------------------------------------

   begin
      return Object.Video_ID;
   end Get_Video_ID;

   -------------------------------------------------------------------------
   function Get_Video_Lib_Constant_Nested_Options (
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video_Lib_Nested_Options_Constant_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Trace_Conversions, "from " & From);
      return Video_Lib_Nested_Options_Constant_Class_Access (
         Ada_Lib.Options.Verification.Get_Ada_Lib_Constant_Nested_Options);
   end Get_Video_Lib_Constant_Nested_Options;

   ---------------------------------------------------------------
   function Has_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Log_Pre_Post (Cameras (Camera_ID) /= Null, Debug,
         "camera id" & Camera_ID'img);
   end Has_Camera;

   ---------------------------------------------------------------
   function Image (
      Value                      : in     Data_Type
   ) return String is
   ---------------------------------------------------------------

   begin
      return Hex_IO.Hex (Interfaces.Integer_8 (Value), 8);
   end Image;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Video_Lib_Nested_Options_Type;
     From                        : in     String := Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options,
         Tag_Name ("options",
            Video_Lib_Nested_Options_Type'class (Options)'tag) &
         " from " & From);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);

      return Log_Out (Ada_Lib.Options.Verification.
            Verification_Nested_Options_Type (Options).Initialize,
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------
   procedure Initialize (
      Object      :    out Object_Type;
      Video_ID    : in     Video_ID_Type) is
   ----------------------------------------------------------------

   begin
      Object.Video_ID := Video_ID;
   end Initialize;

   -------------------------------------------------------------------------
   function Is_Valid (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Camera_ID /= Camera_ID_Not_Set;
   end Is_Valid;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options  : in out Video_Lib_Nested_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Log_Id   : constant Boolean := Debug or Trace_Options;

   begin
      Log_In (Log_Id, Option.Image);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Kind is

            when Ada_Lib.Options.Plain =>

               case Option.Option is

                  when Debug_Option =>
                     Options.Trace_Parse (Iterator);

                  when 'd' =>
                     Options.Directory.Construct (Iterator.Get_Parameter);

                  when Others =>
                     Log_Exception (Log_Id);
                     raise Failed with "Has_Option incorrectly passed " & Option.Image;

               end case;

            when Ada_Lib.Options.Modified =>

               case Option.Option is

                  when 'N' =>
not_implemented;
--                   Video.Object.No_Camera;

                  when others =>
                     Log_Exception (Trace_Options or Debug, " other option" &
                        Option.Image);
                     raise Failed with "Has_Option incorrectly passed " &
                        Option.Image;
               end case;

            when Ada_Lib.Options. Nil_Option =>
               pragma Assert (False, "unexpected kind");

         end case;
         return Log_Out (True, Log_Id, " option" & Option.Image & " handled");
      else
         return Log_Out (False, Log_Id, "other " & Option.Image);
      end if;
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Video_Lib_Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Video Lib";

   begin
      Log_In (Debug or Trace_Options, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program_Mode =>
         Log_Here (Debug or Trace_Options, Quote ("Component", Component));

         Ada_Lib.Help.Create_Option (Directory_Option, False, "directory",
            "current directory", Component, Ada_Lib.Options.Unmodified_Flag);
         Ada_Lib.Help.Create_Option ('r', False, "", "remote camera",
            Component, Ada_Lib.Options.Unmodified_Flag);
         Ada_Lib.Help.Create_Option (Debug_Option, True, "trace options",
            "trace options", Component, Ada_Lib.Options.Unmodified_Flag);
         Ada_Lib.Help.Create_Option ('N', False, "",
            "No Camera attached", Component, Ada_Lib.Help.Modifier);
         New_Line;

      when Ada_Lib.Options.Trace_Mode =>
         Ada_Lib.Help.Set_Has_Trace (Debug_Option, Ada_Lib.Options.Unmodified_Flag);
         New_Line;

         Put_Line (Component & " trace options (-" & Debug_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      d               Video.Lib.Debug: Library_Debug");
         Put_Line ("      D               Video.Object.Debug");
         Put_Line ("      p               Camera.Preset.Debug" &
            " (Video_Options.Presets_Debug");
         Put_Line ("      s               Video.Lib.Debug" &
            " (Video_Options.Configuration_State_Debug)");

      end case;

--    Ada_Lib.Options.Verification.Verification_Nested_Options_Type  (
--       Options).Program_Help (Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Register_Brand (
      Brand                      : in     String;
      Allocator                  : in     Allocator_Type) is
   ----------------------------------------------------------------------------

   begin
not_implemented;
   end Register_Brand;

   ----------------------------------------------------------------------------
   procedure Set_Camera (
      Camera_ID      : in     Camera_Index_Type;
      Camera         : in     Camera_Class_Access) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, "camera id" & Camera_ID'img);
      Cameras (Camera_ID) := Camera;
   end Set_Camera;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Video_Lib_Nested_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log_In (Trace_Options or Debug, Quote ("Parameter", Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug, Quote ("Trace", Trace));

         case Trace is

            when 'a' =>
               Video.Lib.Video_Options.Configuration_State_Debug := True;
               Video.Lib.Video_Options.Library_Debug := True;
               Video.Lib.Video_Options.Object_Debug := True;
               Video.Lib.Video_Options.Presets_Debug := True;

            when 'd' =>
               Video.Lib.Video_Options.Library_Debug := True;

            when 'D' =>
               Video.Lib.Video_Options.Object_Debug := True;

            when 'p' =>
               Video.Lib.Video_Options.Presets_Debug := True;

            when 's' =>
               Video.Lib.Video_Options.Configuration_State_Debug := True;

            when others =>
               Options.Bad_Option (Quote (
                  "unexpected trace option", Trace) &
                  Quote (" for", Debug_Option));

         end case;
      end loop;
      Log_Out (Trace_Options or Debug);
   end Trace_Parse;

begin
--Elaborate := True;
   Debug := Ada_Lib.Options.Ada_Lib_Options.Debug_All;
--Debug_Option := True;
--Trace_Options := True;
   Log_Here (Elaborate or Debug or Trace_Options);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Video.Lib;


