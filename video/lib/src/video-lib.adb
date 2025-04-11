--with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
with ADA_LIB.OS;
with Ada_Lib.Parser;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Socket_IO.Stream_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Configuration.State;
with Debug_Options;
with Interfaces;

package body Video.Lib is

   use type Index_Type;

   Debug_Option                  : constant Character := 'V';
   Options_Debug                 : Boolean := False;
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Create_Options (
                                          "V", Ada_Lib.Options.Unmodified);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Null_Options;

   ---------------------------------------------------------------
   function Address_Kind (
     Options                     : in     Options_Type
   ) return Configuration.Address_Kind_Type is
   ---------------------------------------------------------------

   begin
      return (case Options_Constant_Class_Access (
         Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options).Location is
         when Configuration.State.Remote =>
            Configuration.URL,

         when Configuration.State.Local =>
            Configuration.IP);
   end Address_Kind;

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
     Options                     : in out Options_Type;
     From                        : in     String := Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (Options_Debug or Trace_Options, "from " & From);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);

      return Log_Out (Ada_Lib.Options.Actual.Nested_Options_Type (
            Options).Initialize,
         Options_Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------
   function Parse_Image_Value (
      Value                      : in     String;
      Preset                     :    out Camera_Preset_Type
   ) return String is
   ----------------------------------------------------------------

      Iterator                   : Ada_Lib.Parser.Iterator_Type :=
                                    Ada_Lib.Parser.Initialize (Value, ",");
   begin
      Preset := Camera_Preset_Type (Iterator.Get_Number (True));
      return Iterator.Get_Value (False);
   end Parse_Image_Value;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Options_Debug or Trace_Options, Option.Image);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is

            when Debug_Option =>
               Options.Trace_Parse (Iterator);

            when Others =>
               Log_Exception (Options_Debug or Trace_Options);
               raise Failed with "Has_Option incorrectly passed " & Option.Image;

         end case;

         return Log_Out (True, Options_Debug or Trace_Options,
            " option" & Option.Image & " handled");
      else
         return Log_Out (Ada_Lib.Options.Actual.Nested_Options_Type (
            Options).Process_Option (Iterator, Option),
            Trace_Options or Options_Debug, "other option" & Option.Image);
      end if;
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Video Lib";

   begin
      Log_In (Options_Debug or Trace_Options, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Log_Here (Options_Debug or Trace_Options, Quote ("Component", Component));

         Ada_Lib.Help.Add_Option (Debug_Option, "trace options",
            "trace options", Component);
         New_Line;

      when Ada_Lib.Options.Traces =>
         New_Line;

         Put_Line (Component & " trace options (-" & Debug_Option & ")");
         Put_Line ("      a               all");
--       Put_Line ("      c               Configuration.State debug");
         Put_Line ("      l               library debug");
         Put_Line ("      o               options debug");
--       Put_Line ("      s               Trace simulator");

      end case;

      Ada_Lib.Options.Actual.Nested_Options_Type (Options).Program_Help (Help_Mode);
      Log_Out (Options_Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;

   begin
      Log (Trace_Options or Options_Debug, Here, Who & Quote (" Parameter", Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Options_Debug, Quote ("Trace", Trace));
         case Trace is

            when 'a' =>
               Debug := True;
               Options_Debug := True;

--          when 'C' =>

            when 'l' =>
               Debug := True;

            when 'o' =>
               Options_Debug := True;

            when others =>
               Options.Bad_Option (Quote (
                  "unexpected trace option", Trace) &
                  Quote (" for", Debug_Option));

         end case;
      end loop;
   end Trace_Parse;

begin
--Elaborate := True;
   Options_Debug := Debug_Options.Debug_All;
--Debug := True;
--Trace_Options := True;
   Log_Here (Elaborate or Options_Debug or Debug or Trace_Options);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Video.Lib;


