--with Ada.Unchecked_Deallocation;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
with ADA_LIB.OS;
with Ada_Lib.Parser;
with Ada_Lib.Options.Create;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Configuration.State;
-- with Debug_Options;
with Interfaces;

pragma Elaborate (Ada_Lib.Parser);

package body Video.Lib is

   use type Ada_Lib.Options.Flag_List_Type;
   use type Index_Type;

   Debug_Option                  : constant Character := 'V';
   Debug                         : Boolean renames Video_Options.Library_Debug;
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Flag_List_Type :=
                                       Ada_Lib.Options.Create.Create_One (
                                          'V', Ada_Lib.Options.Unmodified_Flag) &
                                       Ada_Lib.Options.Create.Create_One (
                                          'D', Ada_Lib.Help.Modifier);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Flag_List_Type :=
                                       Ada_Lib.Options.Create.Create_Multiple (
                                          "rS", Ada_Lib.Options.Unmodified_Flag);
   Presets                       : array (Which_Preset_Type) of
                                    Preset_ID_Type := (
                                       others => (
                                          Is_Set      => False,
                                          ID          => 0));

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

   ---------------------------------------------------------------
   function Constructor (
      ID                         : in     Preset_Range_Type
   ) return Preset_ID_Type is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "ID" & ID'img);
      return (
         ID       => ID,
         Is_Set   => True);
   end Constructor;

   ---------------------------------------------------------------
   procedure Dump (
      Prefix_ID                  : in     Preset_ID_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

   begin
      Put_Line ((if What'length > 0 then
            What & " "
         else
            "") &
         "prefix id " & Prefix_ID.Image);
   end Dump;

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
   function Get_Default_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Default_Preset " & Presets (Default_Preset).Image);
      return Presets (Default_Preset);
   end Get_Default_Preset_ID;

   -------------------------------------------------------------------------
   function Get_First_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "First_Presett " & Presets (First_Preset).Image);
      return Presets (First_Preset);
   end Get_First_Preset_ID;

   -------------------------------------------------------------------------
   function Get_ID (
      Preset_ID                  : in     Preset_ID_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Ada_Lib.Socket_IO.Data_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID " & Preset_ID.Image & " from " & From);
      return Preset_ID.ID;
   end Get_ID;

   -------------------------------------------------------------------------
   function Get_Last_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Last preset " & Presets (Last_Preset).Image);
      return Presets (Last_Preset);
   end Get_Last_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Power_On_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Power_On_Preset " & Presets (Power_On_Preset).Image);
      return Presets (Power_On_Preset);
   end Get_Power_On_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Which_Preset " & Which_Preset'img &
         " Preset_ID " & Presets (Which_Preset).Image);
      return Presets (Which_Preset);
   end Get_Preset;

   -------------------------------------------------------------------------
   function Get_Video_Lib_Read_Only_Nested_Options (
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Options_Constant_Class_Access is
   pragma Unreferenced (From);
   -------------------------------------------------------------------------

   begin
      Log_Here (Trace_Conversions, "from " & From);
not_implemented;
return null;
   end Get_Video_Lib_Read_Only_Nested_Options;

   -------------------------------------------------------------------------
   function Have_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Boolean is
   -------------------------------------------------------------------------

      Result         : constant Boolean := Presets (Which_Preset).Is_Set;

   begin
      return Log_Here (Result,
         Debug or Trace_Pre_Post_Conditions or not Result,
         "Preset_ID " & Presets (Which_Preset).Image);
   end Have_Preset;

   ---------------------------------------------------------------
   function Image (
      Preset_ID                  : in     Preset_ID_Type
   ) return String is
   ---------------------------------------------------------------

   begin
      return "id" & Preset_ID.ID'img & " Is_Set " & Preset_ID.Is_Set'img;
   end Image;

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
      Log_In (Debug or Trace_Options, "from " & From);

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

   -------------------------------------------------------------------------
   function Is_Set (
      Preset_ID                  : in     Preset_ID_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Log_Here (Preset_ID.Is_Set,
         Debug or Trace_Pre_Post_Conditions,
         "Preset_ID " & Preset_ID.Image & " from " & From);
   end Is_Set;

   ----------------------------------------------------------------
   function Parse_Image_Value (
      Value                      : in     String;
      Preset                     :    out Preset_ID_Type'class
   ) return String is
   ----------------------------------------------------------------

      Iterator                   : Ada_Lib.Parser.Iterator_Type :=
                                    Ada_Lib.Parser.Initialize (Value, ",");
   begin
      Preset := Constructor (
         Preset_Range_Type (Iterator.Get_Number (True)));
      return Iterator.Get_Value (False);
   end Parse_Image_Value;

   ----------------------------------------------------------------------------
   overriding
   procedure Post_Process (      -- final initialization
     Options                    : in out Video_Lib_Nested_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options,
         "location " & Options.Location'img &
         " options post processing completed");

--    case Options.Location is
--
--       when others =>
--
--    end case;

      Ada_Lib.Options.Verification.Verification_Nested_Options_Type (
         Options).Post_Process;
   end Post_Process;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options  : in out Video_Lib_Nested_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, Option.Image);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
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
               Log_Exception (Debug or Trace_Options);
               raise Failed with "Has_Option incorrectly passed " & Option.Image;

         end case;

         return Log_Out (True, Debug or Trace_Options,
            " option" & Option.Image & " handled");
      else
         return Log_Out (False,Trace_Options or Debug, "other " & Option.Image);
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

         Ada_Lib.Help.Create_Option ('d', "directory", "current directory",
            Component, Ada_Lib.Help.Unmodified_Flag);
--       Ada_Lib.Help.Create_Option ('p', "port option",
--          "port option", Component, Ada_Lib.Help.Unmodified_Flag);
         Ada_Lib.Help.Create_Option ('r', "", "remote camera", Component, Ada_Lib.Help.Unmodified_Flag);
--       Ada_Lib.Help.Create_Option ('s', "", "simulate camera", Component, Ada_Lib.Help.Unmodified_Flag);
         Ada_Lib.Help.Create_Option (Debug_Option, "trace options",
            "trace options", Component, Ada_Lib.Help.Unmodified_Flag);
         New_Line;

      when Ada_Lib.Options.Trace_Mode =>
         New_Line;

         Put_Line (Component & " trace options (-" & Debug_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      d               Debug_Option");
         Put_Line ("      s               configuration.state.Debug");

      end case;

      Ada_Lib.Options.Verification.Verification_Nested_Options_Type'class (
         Options).Program_Help (Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Set (
      Preset_ID                  : in out Preset_ID_Type;
      ID                         : in     Preset_Range_Type) is
   ----------------------------------------------------------------------------

   begin
      Preset_ID.ID := ID;
   end Set;

   ----------------------------------------------------------------------------
   procedure Set_Preset_ID (
      Which_Preset               : in     Which_Preset_Type;
      Preset_ID                  : in     Preset_ID_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "which " & Which_Preset'img &
         Preset_ID.Image);
      Presets (Which_Preset) := Preset_ID;
      Log_Out (Debug);
   end Set_Preset_ID;

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


