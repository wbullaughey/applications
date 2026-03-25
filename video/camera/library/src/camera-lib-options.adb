--with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Help;
with Ada_Lib.Options.Create;
--with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with ADA_LIB.OS;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Command_Name;
--with Video.Lib;

package body Camera.Lib.Options is

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;
-- use type Ada_Lib.Options.Flag_List_Type;

   Debug_Options           : Boolean renames Camera_Options.Options_Debug;
   Trace_Option            : constant Character := 'T';
   Options_With_Parameters : aliased constant
                              Ada_Lib.Options.Flag_List_Type :=
                                 Ada_Lib.Options.Create.Create_One (
                                    Trace_Option, Ada_Lib.Options.
                                       Unmodified_Flag);
   Recursed                : Boolean := False;

   -------------------------------------------------------------------------
   function Current_Directory  -- set by runstring option 'c' else null
   return String is
   -------------------------------------------------------------------------

   begin
      return Get_Camera_Readonly_Options.Directory.Coerce;
   end Current_Directory;

   -------------------------------------------------------------------------
   function Get_Camera_Lib_Options_Read_Only_Nested_Options (
      From                       : in     String
   ) return Nested_Options_Constant_Class_Access is
   -------------------------------------------------------------------------

   begin
not_implemented;
return null;
   end Get_Camera_Lib_Options_Read_Only_Nested_Options;

-- -------------------------------------------------------------------------
-- function Has_Camera
-- return Boolean is
-- -------------------------------------------------------------------------
--
-- begin
--    Log_In (Debug_Options);
--    declare
--       Options  : Ada_Lib.Options.
--                   Abstract_Runtime_Options_Constant_Class_Access :=
--                      Ada_Lib.Options.Verification.Get_Ada_Lib_Read_Only_Nested_Options;
--    begin
--       if Debug_Options then
--          Tag_History (Options.all'tag);
--       end if;
--
--       declare
--          Program_Options   : Program_Options_Constant_Class_Access renames
--                               Program_Options_Constant_Class_Access (
--                                  Options);
--       begin
--          return Log_Out (Program_Options.Nested_Options.Location =
--                   Video.Lib.Remote, Debug_Options);
--       end;
--    end;
--
-- exception
--    when Fault: others =>
--       Trace_Exception (Fault);
--       raise;
--
-- end Has_Camera;

   -------------------------------------------------------------------------
   overriding
   function Image (
     Options                     : in     Camera_Lib_Options_Nested_Options_Type
   ) return String is
   -------------------------------------------------------------------------

   begin
not_implemented;
return "";
   end Image;
   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Camera_Lib_Options_Nested_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Recursed, Debug_Options or Trace_Options,
         "from " & From & " options address " & Image (Options'address));

      return Log_Out_Checked (Recursed,
         Camera_Lib_Nested_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options, "from " & From & " options address " &
         Image (Options'address));
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Ada_Lib.Options.Null_Flag_List);

      return Log_Out (
         Options.Nested_Options.Initialize and then
         Ada_Lib.Options.Program.Program_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

--   ----------------------------------------------------------------------------
--   -- processes options it knows about and calls parent for others
--   overriding
--   function Process_Option (
--      Options  : in out Camera_Lib_Options_Nested_Options_Type;
--      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
--      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
--   ) return Boolean is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_In (Trace_Options or Debug_Options, Option.Image);
--not_implemented;
--
--      return Log_Out (True, Trace_Options or Debug_Options, "exit" & " option" &
--         Option.Image & " handled");
--   end Process_Option;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options  : in out Program_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image);
not_implemented;

--    if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
--          Ada_Lib.Options.Null_Flag_List) then
--       case Option.Option is
--          when Trace_Option =>
--             declare
--                Parameter
--                         : constant String := Iterator.Get_Parameter;
--             begin
--                Log (Trace_Options or Debug_Options, Here, " process parameter  " & Quote (Parameter));
--
--                for Trace of Parameter loop
--                   Log_Here (Trace_Options or Debug_Options, Quote ("Trace", Trace));
--                   case Trace is
--
--                      when 'a' =>
--                         Debug_Options := True;
--                         Options.Debug := True;
--
--                      when 'm' =>
--                         Options.Debug := True;
--
--                      when 'r' =>
--                         Debug_Options := True;
--
--                      when 's' =>
--                         Camera_Options.State_Debug := True;
--
--                      when others =>
--                         Log_Out (Debug_Options);
--                         Options.Bad_Option (Quote (
--                            "unexpected trace option", Trace) &
--                            " for '" & Trace_Option & "'");
--                         return False;
--
--                   end case;
--                end loop;
--             end;
--
--          when others =>
--             declare
--                Message  : constant String :=
--                            "Has_Option incorrectly passed " & Option.Image;
--             begin
--                Log_Exception (Trace_Options or Debug_Options, Message);
--                raise Failed with Message;
--             end;
--
--       end case;
--    else
--       Log_Out (Trace_Options or Debug_Options, "other " & Option.Image);
--       return Options.GNOGA.Process_Option (Iterator, Option) or else
--          Options.Nested_Options.Process_Option (Iterator, Option) or else
--          Ada_Lib.Options.Program.Program_Options_Type (Options).Process_Option (
--             Iterator, Option);
--    end if;

      return Log_Out (True, Trace_Options or Debug_Options, "exit" & " option" &
         Option.Image & " handled");
   end Process_Option;

--   ----------------------------------------------------------------------------
--   overriding
--   procedure Program_Help (
--      Options                    : in     Camera_Lib_Options_Nested_Options_Type;  -- only used for dispatch
--      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img);
--not_implemented;
--   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------
--
--    Component                  : constant String := "Camera.Lib.Options";
--
   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img);
not_implemented;
--
--    case Help_Mode is
--
--    when Ada_Lib.Options.Program_Mode =>
--       Ada_Lib.Help.Create_Option (Trace_Option,
--          "trace options", "trace options", Component, Ada_Lib.Help.Unmodified_Flag);
--
--    when Ada_Lib.Options.Trace_Mode =>
--       New_Line;
--
--       Put_Line (Command_Name & " trace options -" &
--          Trace_Option & ")");
--       Put_Line ("      a               all");
--       Put_Line ("      m               main program options");
--       Put_Line ("      r               runtime options");
--       Put_Line ("      s               Camera.Configuration.Debug options");
--       New_Line;
--
--    end case;
--
--    Ada_Lib.Options.Program.Program_Options_Type (Options).Program_Help (Help_Mode);
--    Options.GNOGA.Program_Help (Help_Mode);
--    Options.Nested_Options.Program_Help (Help_Mode);
      Log_Out (Debug_Options or Trace_Options);
   end Program_Help;

-- ----------------------------------------------------------------------------
-- procedure Set_Protected_Options (
--    Options  : in not null Ada_Lib.Options.Flags.
--                            Program_Options_Class_Access) is
-- ----------------------------------------------------------------------------
--
-- begin
--    Protected_Options := Options;
--    Ada_Lib.Options.Set_Ada_Lib_Options (
--       Ada_Lib.Options.Interface_Options_Class_Access (Options));
-- end Set_Protected_Options;

begin
--Protected_Options.Debug := True;
--Debug := True;
--Trace_Options := True;
--Elaborate := True;

   Log_Here (Elaborate or Debug_Options or Trace_Options);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera.Lib.Options;
