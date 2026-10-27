--with Ada.Unchecked_Deallocation;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
with ADA_LIB.OS;
--with Ada_Lib.Parser;
--with Ada_Lib.Options.Create;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Video.Configuration;
-- with Debug_Options;
with Interfaces;

--pragma Elaborate (Ada_Lib.Parser);

package body Video.Lib is

   use type Ada_Lib.Options.Flag_List_Type;
   use type Index_Type;

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
-- Presets     : array (Which_Preset_Type) of
--                Preset_ID_Type := (
--                   others => (
--                      Camera   => Null,
--                      ID       => 0,
--                      Is_Set   => False));

-- ---------------------------------------------------------------
-- function Address_Kind (
--   Options                     : in     Video_Lib_Nested_Options_Type
-- ) return Address_Kind_Type is
-- ---------------------------------------------------------------
--
-- begin
--    return (case Options.Location is
--       when Video.Lib.Remote => URL,
--
--       when Video.Lib.Local => IP,
--
--       when Video.Lib.No_Location => NOT_SET);
-- end Address_Kind;

   -------------------------------------------------------------------------
   function Camera_Configured
   return Boolean is
   -------------------------------------------------------------------------

      Video_Lib_Options
                  : constant Video_Lib_Nested_Options_Constant_Class_Access :=
                     Get_Video_Lib_Read_Only_Nested_Options;
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
      Ada_Lib.Socket_IO.Stream_IO.Dump (Description,
         Buffer (Buffer'first .. Buffer'first + Index_Type (Length) - 1), From);
   end Dump;

   -------------------------------------------------------------------------
   function Get_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Camera_Class_Access is
   -------------------------------------------------------------------------

   begin
      return Cameras (Camera_ID);
   end Get_Camera;

   -------------------------------------------------------------------------
   function Get_Video_Lib_Read_Only_Nested_Options (
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video_Lib_Nested_Options_Constant_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Trace_Conversions, "from " & From);
      return Video_Lib_Nested_Options_Constant_Class_Access (
         Ada_Lib.Options.Verification.Get_Ada_Lib_Read_Only_Nested_Options);
   end Get_Video_Lib_Read_Only_Nested_Options;

   ---------------------------------------------------------------
   function Has_Camera (
      Camera_ID                  : in     Camera_ID_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Cameras (Camera_ID) /= Null;
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

-- ----------------------------------------------------------------
-- function Parse_Image_Value (
--    Value                      : in     String;
--    Preset                     :    out Preset_ID_Type'class
-- ) return String is
-- ----------------------------------------------------------------
--
--    Iterator                   : Ada_Lib.Parser.Iterator_Type :=
--                                  Ada_Lib.Parser.Initialize (Value, ",");
-- begin
--    Preset := Constructor (
--       Preset_ID_Type (Iterator.Get_Number (True)));
--    return Iterator.Get_Value (False);
-- end Parse_Image_Value;

--   ----------------------------------------------------------------------------
--   overriding
--   procedure Post_Process (      -- final initialization
--     Options                    : in out Video_Lib_Nested_Options_Type) is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_Here (Debug or Trace_Options,
--         "location " & Options.Location'img &
--         " options post processing completed");
--
----    case Options.Location is
----
----       when others =>
----
----    end case;
--
--      Ada_Lib.Options.Verification.Verification_Nested_Options_Type  (
--         Options).Post_Process;
--   end Post_Process;

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

      --          when 'p' => get it from configuration state
      --             Options.Port_Number := Port_Type (
      --                Ada_Lib.Socket_IO.Port_Type (Iterator.Get_Integer));

                  when 'r' =>    -- remote camera
                     Options.Location := Remote;

      --          when 'S' =>    -- simulate Standard.Camera
      --             if    Options.Location = Remote and then
      --                   not Ada_Lib.Options.Ada_Lib_Environment.Help_Test then
      --                Options.Bad_Option (
      --                   "Remote option (r) and Simulate (E) are incompatable at " &
      --                   Here);
      --             end if;
      --             Options.Simulate := True;

                  when Others =>
                     Log_Exception (Log_Id);
                     raise Failed with "Has_Option incorrectly passed " & Option.Image;

               end case;

            when Ada_Lib.Options.Modified =>

               case Option.Option is

                  when 'N' =>
                     Video.Configuration.No_Camera;

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
         Put_Line ("      D               Video.Configuration.Debug");
         Put_Line ("      s               " &
            "Video.Lib.Debug: Configuration_State_Debug");

      end case;

--    Ada_Lib.Options.Verification.Verification_Nested_Options_Type  (
--       Options).Program_Help (Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

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

            when 'd' =>
               Video.Lib.Video_Options.Library_Debug := True;

            when 'D' =>
               Video.Lib.Video_Options.Configuration_Debug := True;

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


