--with Ada.Command_Line;
--with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Help;
--with Ada_Lib.Options.GNOGA;
--with ADA_LIB.Strings.Unlimited;
--with Ada_Lib.Test;
with ADA_LIB.OS;
with Ada_Lib.Runstring_Options;
--with ADA_LIB.Text;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Command_Name;
--with Configuration.Camera.State;
--with Configuration.State;
--with Controller;
--with Debug_Options;
--with Emulator;
--with Gnoga.Gui.Window;
--with Main;
--with Video.Lib;
--with Camera.Commands.PTZ_Optics;
--with Widgets.Control;
--with Widgets.Configured;
--with Widgets.Video;
--with Windows.Top;

package body Camera.Lib.Options is

-- Configuration_Option          : constant Character := 'c';
-- Template_Option               : constant Character := 't';
   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options_Interface.Options_Type :=
                                       Ada_Lib.Options_Interface.Create_Options (
                                          Trace_Option);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options_Interface.Options_Type :=
                                       Ada_Lib.Options_Interface.Null_Options;
   Protected_Options             : aliased Options_Type;

   -------------------------------------------------------------------------
   function Current_Directory  -- set by runstring option 'c' else null
   return String is
   -------------------------------------------------------------------------

   begin
      return Protected_Options.Camera_Library.Directory.Coerce;
   end Current_Directory;

   -------------------------------------------------------------------------
   function Get_Modifyable_Options return Options_Access is
   -------------------------------------------------------------------------

   begin
      return Protected_Options'access;
   end Get_Modifyable_Options;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options, "from " & From & " options address " &
         Image (Options'address) &
         " protected options address " & Image (Protected_Options'address));
      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.Without_Parameters,
         Options_Without_Parameters);

--    Configuration.Camera.State.Global_Camera_State :=
--       new Configuration.Camera.State.State_Type;

      return Log_Out (
         Options.GNOGA.Initialize and then
         Options.Camera_Library.Initialize and then
         Ada_Lib.Options.Program_Options_Type (Options).Initialize and then
         Options.Process (
            Include_Options      => True,
            Include_Non_Options  => False,
            Modifiers            => String'(
               1 => Ada_Lib.Help.Modifier)),
         Debug_Options or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Options_Type;
      Iterator                   : in out ADA_LIB.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image);

      if Ada_Lib.Options_Interface.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is
            when Trace_Option =>
               declare
                  Parameter
                           : constant String := Iterator.Get_Parameter;
               begin
                  Log (Trace_Options or Debug_Options, Here, " process parameter  " & Quote (Parameter));

                  for Trace of Parameter loop
                     Log_Here (Trace_Options or Debug_Options, Quote ("Trace", Trace));
                     case Trace is

                        when 'a' =>
                           Debug_Options := True;
                           Options.Debug := True;

                        when 'm' =>
                           Options.Debug := True;

                        when 'r' =>
                           Debug_Options := True;

                        when others =>
                           Log_Out (Debug_Options);
                           Options.Bad_Option (Quote (
                              "unexpected trace option", Trace) &
                              " for '" & Trace_Option & "'");
                           return False;

                     end case;
                  end loop;
               end;

            when others =>
               Log_Exception (Debug_Options or Trace_Options);
               raise Failed with "Has_Option incorrectly passed" & Option.Image;

         end case;
      else
         Log_Out (Trace_Options or Debug_Options, "other " & Option.Image);
         return Options.GNOGA.Process_Option (Iterator, Option) or else
            Options.Camera_Library.Process_Option (Iterator, Option) or else
            ADA_LIB.Options.Program_Options_Type (Options).Process_Option (
               Iterator, Option);
      end if;

      return Log_Out (True, Trace_Options or Debug_Options, "exit" & " option" &
         Option.Image & " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Camera.Lib.Options";

   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option (Trace_Option,
            "trace options", Component);

      when Ada_Lib.Options.Traces =>
         New_Line;

         Put_Line (Command_Name & " trace options -" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      m               main program options");
         Put_Line ("      r               runtime options");
         New_Line;

      end case;

      Ada_Lib.Options.Program_Options_Type (Options).Program_Help (Help_Mode);
      Options.GNOGA.Program_Help (Help_Mode);
      Options.Camera_Library.Program_Help (Help_Mode);
      Log_Out (Debug_Options or Trace_Options);
   end Program_Help;

begin
-- Debug := Debug_Options.Debug_All;

--Protected_Options.Debug := True;
--Debug := True;
--Trace_Options := True;
--Elaborate := True;

   Ada_Lib.Options_Interface.Set_Ada_Lib_Options (Protected_Options'access);
   Log_Here (Elaborate or Debug_Options or Trace_Options, "options " &
      Image (Protected_Options'address));

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera.Lib.Options;
