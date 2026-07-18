--with Ada.Exceptions;
with Ada_Lib.OS;
with Ada_Lib.Options; -- use  Ada_Lib.Options;
--with Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Camera.Lib.Options.Unit_Test is

   Debug                   : Boolean renames
                              Camera_Lib_Unit_Test.Unit_Test_Debug;
-- Initialize_Recursed     : Boolean := False;
   Process_Option_Recursed : Boolean := False;
-- Program_Help_Recursed   : Boolean := False;

--   ----------------------------------------------------------------------------
--   overriding
--   procedure Display_Help (
--     Options   : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
--     Message   : in     String := "";   -- leave blank no error help
--     Halt      : in     Boolean := True) is
--   ----------------------------------------------------------------------------
--
--   begin
--log_here;
--   end Display_Help;

   ----------------------------------------------------------------------------
   function Get_Modifiable_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Trace_Conversions, "from " & From);
not_implemented;
return null;
   end Get_Modifiable_Camera_Unit_Test_Options;

   ----------------------------------------------------------------------------
   function Get_Read_Only_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
      Log_Here (Trace_Conversions, "from " & From);
not_implemented;
return null;
   end Get_Read_Only_Camera_Unit_Test_Options;

   ----------------------------------------------------------------------------
   function Image (
     Options                     : in     Camera_Unit_Test_Program_Options_Type
   ) return String is
   ----------------------------------------------------------------------------

   begin
not_implemented;
return "";
   end Image;

--   ----------------------------------------------------------------------------
--   overriding
--   function Initialize (
--     Options                     : in out Camera_Unit_Test_Program_Options_Type;
--     From                        : in     String := Standard.Ada_Lib.Trace.Here
--   ) return Boolean is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_In_Checked (Initialize_Recursed, Debug or Trace_Options,
--         Tag_Name ("options", Camera_Unit_Test_Program_Options_Type'class (
--            Options)'tag));
--
--      return Log_Out_Checked (Initialize_Recursed,
----           Options.Camera_Lib_Nested_Options.Initialize (From) and then
----           Options.Nested_Ada_Lib_Unit_Test_Options.Initialize (From) and then
--             Camera.Lib.Unit_Test.Camera_Lib_Unit_Test_Program_Options_Type (
--               Options).Initialize (From),
--             Debug or Trace_Options);
--   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process (     -- process command line options
     Options   : in out Camera_Unit_Test_Program_Options_Type;
     Iterator  : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean is
   ----------------------------------------------------------------------------

--begin
--log_here;
--declare
      Nested_Options : constant Ada_Lib.Options.Verification.
                           Verification_Nested_Options_Class_Access :=
                        Ada_Lib.Options.Verification.
                           Get_Ada_Lib_Modifiable_Nested_Options;
   begin
      Log_In (Debug or Trace_Options,
         Tag_Name ("Nested_Options", Nested_Options.all'tag));
--tag_history ("Nested_Options", Nested_Options.all'tag);

      while not Iterator.At_End loop
         begin
            if Iterator.Is_Option then
               declare
                  Option   : constant Ada_Lib.Options.
                              Flag_Option_Type'class :=
                                 Iterator.Get_Option;
                  Message  : constant String := Option.Image & " not defined";

               begin
                  Log_Here (Debug or Trace_Options, Option.Image);
                  if    -- Options.Camera_Lib_Nested_Options.Process_Option (
--                         Iterator, Option) or else
--                      Options.Nested_Ada_Lib_Unit_Test_Options.
--                         Process_Option (Iterator, Option) or else
                        Camera.Lib.Unit_Test.Camera_Lib_Unit_Test_Program_Options_Type (
                              Options).Process_Option (Iterator, Option)  then
                     Log_Here (Debug or Trace_Options, Option.Image, "processed");
                  else
                     Log_Here (Debug or Trace_Options, Message);
--                   Options.Bad_Option (Option, Message);     -- aborts program
--                   return Log_Out (False, Debug or Trace_Options);
                     return Log_Out (Camera.Lib.Unit_Test.
                        Camera_Lib_Unit_Test_Program_Options_Type (
                           Options).Process (Iterator),
                        Debug or Trace_Options);
                  end if;
               end;
            else
               declare
                  Argument       : constant String :=
                                    Iterator.Get_Argument;
               begin
                  if not Nested_Options.Process_Argument (
                        Iterator, Argument) then
                     Log_Out (Debug or Trace_Options);
                     Options.Bad_Option ("unexpected '" & Argument & "' on run string" &
                        " from " & Here);
                        -- raises exception
                  end if;
               end;
            end if;

         exception

            when Fault: others =>
               Trace_Exception (Debug or Trace_Options, Fault,
                  "help test " &
                     Ada_Lib.Options.Ada_Lib_Environment.Help_Test'img);
--not_implemented;
               if not Ada_Lib.Options.Ada_Lib_Environment.Help_Test then
                  raise;
               end if;

         end;
         if not Iterator.At_End then
            Iterator.Advance;
         end if;
      end loop;

      return Log_Out (True, Debug or Trace_Options, "processed");

   exception

      when Fault: others =>
         Trace_Exception (Debug or Trace_Options, Fault);
         raise;
--end;
   end Process;

--   ----------------------------------------------------------------------------
--   function Process (     -- processes whole command line calling Process_Option for each option
--     Options                     : in out Camera_Unit_Test_Program_Options_Type;
--     Include_Options             : in     Boolean;
--     Include_Non_Options         : in     Boolean;
--     Option_Prefix               : in     Character := '-';
--     Modifiers                   : in     String := ""
--   ) return Boolean is
--   ----------------------------------------------------------------------------
--
--      Log   : constant Boolean := Debug or Trace_Options;
--
--   begin
--      Log_In (Log, "Include_Options " & Include_Options'img &
--         " Include_Non_Options " & Include_Non_Options'img &
--         Ada_Lib.String_Quote.Quote (" modifiers", Modifiers) &
--         Tag_Name ("Options", Camera_Unit_Test_Program_Options_Type'class (
--            Options)'tag));
--
--      declare
--         Iterator                : Ada_Lib.Command_Line_Iterator.Run_String.
--                                    Runstring_Iterator_Type;
--
--      begin
--         Log_Here (Log);
--         Iterator.Initialize (Include_Options, Include_Non_Options,
--            Option_Prefix, Modifiers);
--log_here;
--         if not Options.Process (Iterator) then
--            return Log_Out (False,Log);
--         end if;
--log_here;
--
--      exception
--         when Fault: others =>
--            Trace_Exception (Log, Fault);
--            Verification.Get_Ada_Lib_Read_Only_Program_Options.Display_Help (
--               Ada.Exceptions.Exception_Message (Fault));
--            return Log_Out (False,Log);
--      end;
--
--      Options.Program_Processed := True;
--      return Log_Out (True, Log);
--
--   exception
--      when Fault: Ada_Lib.Options.Failed =>
--         Trace_Exception (Log, Fault);
--         Options.Display_Help (Ada.Exceptions.Exception_Message (Fault), True);
--         raise;
--
--      when Fault: others =>
--         Trace_Exception (Log, Fault);
--         raise;
--
--   end Process;

--   ----------------------------------------------------------------------------
--   overriding
--   procedure Program_Help (
--      Options     : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
--      Help_Mode   : in     Ada_Lib.Options.Help_Mode_Type) is
--   ----------------------------------------------------------------------------
--
--   begin
--      Log_In_Checked (Program_Help_Recursed, Debug or Trace_Options);
--
----    Options.Camera_Lib_Nested_Options.Program_Help (Help_Mode);
----    Options.Nested_Ada_Lib_Unit_Test_Options.Program_Help (Help_Mode);
--      Camera.Lib.Unit_Test.Camera_Lib_Unit_Test_Program_Options_Type (Options).Program_Help (Help_Mode);
--      Log_Out_Checked (Program_Help_Recursed, Debug or Trace_Options);
--   end Program_Help;
--
   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options     : in out Camera_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Log   : constant Boolean := Debug or Trace_Options;

   begin
      Tag_History (Log, "options",Camera_Unit_Test_Program_Options_Type'class (options)'tag);
      Log_In_Checked (Process_Option_Recursed, Log);

      return Log_Out_Checked (Process_Option_Recursed,
--           Options.Camera_Lib_Nested_Options.Process_Option (
--             Iterator, Option) or else
--           Options.Nested_Ada_Lib_Unit_Test_Options.Process_Option (
--             Iterator, Option) or else
             Camera.Lib.Unit_Test.Camera_Lib_Unit_Test_Program_Options_Type (
               Options).Process_Option (Iterator, Option),
             Log);
   end Process_Option;

   ----------------------------------------------------------------------------
   procedure Register_Tests (
      Options     : in     Camera_Unit_Test_Program_Options_Type;
      Suite_Name  : in     String;
      Test        : in out Ada_Lib.Unit_Test.Test_Cases.
                              Test_Case_Type'class) is
   ----------------------------------------------------------------------------

   begin
not_implemented;
   end Register_Tests;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options     : in out Camera_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                              Command_Line_Iterator_Interface'class) is
   pragma Unreferenced (Options, Iterator);
   ----------------------------------------------------------------------------

   begin
not_implemented;
   end Trace_Parse;

begin
   Log_Here (Debug or Elaborate or Trace_Options);


exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera.Lib.Options.Unit_Test;
