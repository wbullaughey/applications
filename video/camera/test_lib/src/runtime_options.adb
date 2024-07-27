with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Options.Help;
--with ADA_LIB.Options;
with ADA_LIB.OS;
with Ada_Lib.Options.Runstring;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Command_Name;
--with Configuration.Camera.State;
with Debug_Options;

package body Runtime_Options is

   Trace_Option                  : constant Character := 'T';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Create_Options (
                                          Trace_Option);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options.Options_Type :=
                                       Ada_Lib.Options.Null_Options;
   Protected_Options             : aliased Options_Type;

-- -------------------------------------------------------------------------
-- function Current_Directory  -- set by runstring option 'c' else null
-- return String is
-- -------------------------------------------------------------------------
--
-- begin
--    return Protected_Options.Camera_Library.Directory.Coerce;
-- end Current_Directory;
--
-- -------------------------------------------------------------------------
-- function Get_Modifyable_Options return Options_Access is
-- -------------------------------------------------------------------------
--
-- begin
--    return Protected_Options'access;
-- end Get_Modifyable_Options;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Options_Type;
     From                        : in     String := Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Options, "from " & From & " options address " &
         Image (Options'address) &
         " protected options address " & Image (Protected_Options'address));
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);

      return Log_Out (
         Camera.Lib.Options_Type (Options).Initialize and then
         Options.Process (
            Include_Options      => True,
            Include_Non_Options  => False,
            Modifiers            => String'(
               1 => Ada_Lib.Options.Help.Modifier)),
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options              : in out Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class;
      Option               : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug, Option.Image);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is
            when Trace_Option =>
               declare
                  Parameter
                           : constant String := Iterator.Get_Parameter;
               begin
                  Log (Trace_Options or Debug, Here, " process parameter  " & Quote (Parameter));

                  for Trace of Parameter loop
                     Log_Here (Trace_Options or Debug, Quote ("Trace", Trace));
                     case Trace is

                        when 'a' =>
                           Debug := True;
--                         Options.Debug := True;

--                      when 'm' =>
--                         Options.Debug := True;
--
--                      when 'r' =>
--                         Debug := True;

                        when others =>
                           Log_Out (Debug);
                           Options.Bad_Option (Quote (
                              "unexpected trace option", Trace) &
                              " for '" & Trace_Option & "'");
                           return False;

                     end case;
                  end loop;
               end;

            when others =>
               Log_Exception (Debug or Trace_Options);
               raise Failed with "Has_Option incorrectly passed" & Option.Image;

         end case;
      else
         Log_Out (Trace_Options or Debug, "other " & Option.Image);
         return Camera.Lib.Options_Type (Options).Process_Option (
            Iterator, Option);
      end if;

      return Log_Out (True, Trace_Options or Debug, "exit" & " option" &
         Option.Image & " handled");
   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Runtime_Options";

   begin
      Log_In (Debug or Trace_Options, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Options.Help.Add_Option (Trace_Option,
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

      Camera.Lib.Options_Type (Options).Program_Help (Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Set_Options (
      Options                    : in     Options_Class_Access;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Debug, Tag_Name (Options_Type'class (Protected_Options)'tag));

      Ada_Lib.Options.Set_Ada_Lib_Options (
         Protected_Options'access);
   end Set_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

   begin
      Not_Implemented;
   end Trace_Parse;

begin

   Debug := Debug_Options.Debug_All;

--Protected_Options.Debug := True;
--Debug := True;
--Trace_Options := True;
--Elaborate := True;

   Ada_Lib.Options.Set_Ada_Lib_Options (Protected_Options'access);
   Log_Here (Elaborate or Debug or Trace_Options, "options " &
      Image (Protected_Options'address));

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Runtime_Options;
