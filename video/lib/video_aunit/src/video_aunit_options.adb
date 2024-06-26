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
--with ADA_LIB.Template;
--with ADA_LIB.Text;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Command_Name;
--with Controller;
with Debug_Options;
--with Emulator;
--with Gnoga.Gui.Window;

package body Runtime_Options is

   Options_With_Parameters       : constant Ada_Lib.Options_Interface.Options_Type :=
                                    Ada_Lib.Options_Interface.Create_Options ('t');
   Options_Without_Parameters    : constant Ada_Lib.Options_Interface.Options_Type :=
                                    Ada_Lib.Options_Interface.Create_Options ('E');
   Protected_Options             : aliased Options_Type;

   -------------------------------------------------------------------------
   function Get_Modifyable_Options return Options_Access is
   -------------------------------------------------------------------------

   begin
      return Protected_Options'access;
   end Get_Modifyable_Options;

   -------------------------------------------------------------------------
   procedure Initialize is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug or Trace_Options);
      if not Protected_Options.Initialize then
         Put_Line ("Could not initialize options");
      end if;
   end Initialize;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Options_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (debug or Trace_Options);
      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.With_Parameters, Options_With_Parameters);
      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.Without_Parameters,
         Options_Without_Parameters);
      return Log_Out (Protected_Options.Unit_Test.Initialize and then
         Video.Lib.Options_Type (Protected_Options).Initialize, Debug) and then
         Protected_Options.Process (
            Include_Options      => True,
            Include_Non_Options  => True,
            Modifiers            => String'(
               1 => Ada_Lib.Help.Modifier));
--    Options.Initialized := True;
--    if not Protected_Options.Set_Options then      -- set any local pointers, calls down derivation tree
--       Put_Line ("Set_Options did not call to root");
--       ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
--    end if;
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
      Log_In (Trace_Options or Debug, Option.Image);

--    if Ada_Lib.Options_Interface.Has_Option (Option, Options_With_Parameters,
      if Option.Is_Registered then
--          Options_Without_Parameters) then
         case Option.Option is

            when 't' =>
               declare
                  Parameter
                           : constant String := Iterator.Get_Parameter;
               begin
                  Log (Debug, Here, " process parameter  " & Quote (Parameter));

                  for Trace of Parameter loop
                     Log_Here (Trace_Options or Debug, Quote ("Trace", Trace));
                     case Trace is

                        when 'a' =>
                           Debug := True;

                        when 'o' =>    -- options
                           Debug := True;

                        when others =>
                           Options.Bad_Option (Quote (
                              "unexpected trace option", Trace));
                           return False;

                     end case;
                  end loop;
               end;

            when others =>
               raise Failed with "Has_Option incorrectly passed" & Option.Image;

         end case;
      else
         return Log_Out (Video.Lib.Options_Type (
            Options).Process_Option (Iterator, Option) or else
            Options.Unit_Test.Process_Option (Iterator, Option),
            Debug, "other option" & " Option" & Option.Image);
      end if;

      return Log_Out (True, Debug, "option" & Option.Image & " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "help mode " & Help_Mode'img);
      Video.Lib.Options_Type (Options).Program_Help (Help_Mode);
      Options.Unit_Test.Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option ('E', "", "simulate video",Command_Name);
         Ada_Lib.Help.Add_Option ('t', "trace options", "trace options", Command_Name);
         New_Line;

      when Ada_Lib.Options.Traces =>
         New_Line;
         Put_Line (Command_Name & " trace options (-t)");
         Put_Line ("      a               all");
         Put_Line ("      m               main unit test trace options");
         Put_Line ("      r               runtime options");

      end case;

   end Program_Help;

begin
--put_line (Here);
--Elaborate := True;
   Debug := Debug_Options.Debug_All;
--Debug := True;
--Trace_Options := True;
   Options := Protected_Options'access;
   Ada_Lib.Options_Interface.Set_Ada_Lib_Options (
      Ada_Lib.Options_Interface.Interface_Options_Class_Access (Options));
   Log_Here (Debug or Trace_Options or Elaborate, "options " & Image (Options.all'address));

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Runtime_Options;
