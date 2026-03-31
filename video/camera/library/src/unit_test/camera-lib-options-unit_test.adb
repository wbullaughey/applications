with Ada_Lib.OS;
--with Ada_Lib.Options.Program;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Camera.Lib.Options.Unit_Test is

   Debug       : Boolean renames Camera_Lib_Unit_Test.Unit_Test_Debug;
   Initialize_Recursed
               : Boolean := False;

   ----------------------------------------------------------------------------
   overriding
   procedure Display_Help (
     Options   : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
     Message   : in     String := "";   -- leave blank no error help
     Halt      : in     Boolean := True) is
   ----------------------------------------------------------------------------

   begin
log_here;
   end Display_Help;

   ----------------------------------------------------------------------------
   function Get_Modifiable_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Class_Access is
   pragma Unreferenced (From);
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
   pragma Unreferenced (From);
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

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Camera_Unit_Test_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In_Checked (Initialize_Recursed, Debug or Trace_Options);

      return Log_Out_Checked (Initialize_Recursed,
             Options.Camera_Lib_Nested_Options.Initialize (From) and then
--           Options.Camera_Lib_Unit_Test_Program_Options.Initialize (
--             From) and then
--           Options.Nested_Unit_Test_Options.Initialize (From) and then
             Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (
               Options).Initialize (From),
             Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process (     -- process command line options
     Options   : in out Camera_Unit_Test_Program_Options_Type;
     Iterator  : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Nested_Options : constant Ada_Lib.Options.Verification.
                           Verification_Nested_Options_Class_Access :=
                        Ada_Lib.Options.Verification.
                           Get_Ada_Lib_Modifiable_Nested_Options;
   begin
      Log_In (Debug or Trace_Options,
         Tag_Name ("Nested_Options", Nested_Options.all'tag));
tag_history ("Nested_Options", Nested_Options.all'tag);

      while not Iterator.At_End loop
         begin
            if Iterator.Is_Option then
               declare
                  Option   : constant Ada_Lib.Options.
                              Base_Flag_Option_Type'class :=
                                 Iterator.Get_Option;
                  Message  : constant String := Option.Image & " not defined";

               begin
                  Log_Here (Debug or Trace_Options, Option.Image);
                  if    Options.Camera_Lib_Nested_Options.Process_Option (
                           Iterator, Option) or else
                        Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (
                              Options).Process_Option (Iterator, Option)  then
                     Log_Here (Debug or Trace_Options, Option.Image, "processed");
                  else
                     Log_Here (Debug or Trace_Options, Message);
--                   Options.Bad_Option (Option, Message);     -- aborts program
--                   return Log_Out (False, Debug or Trace_Options);
                     return Log_Out (Ada_Lib.Options.AUnit_Lib.
                        Aunit_Program_Options_Type (Options).Process (Iterator),
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
               Trace_Exception (Debug or Trace_Options, Fault);
not_implemented;
--             if not Nested_Options.Help_Test then
--                raise;
--             end if;

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

   end Process;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options     : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode   : in     Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In_Checked (Initialize_Recursed, Debug or Trace_Options);

      Options.Camera_Lib_Nested_Options.Program_Help (Help_Mode);
--    Options.Camera_Lib_Unit_Test_Program_Options.Program_Help (Help_Mode);
--    Options.Nested_Unit_Test_Options.Program_Help (Help_Mode);
      Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (Options).Program_Help (Help_Mode);
      Log_Out_Checked (Initialize_Recursed, Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (
      Options     : in out Camera_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In_Checked (Initialize_Recursed, Debug or Trace_Options);

      return Log_Out_Checked (Initialize_Recursed,
             Options.Camera_Lib_Nested_Options.Process_Option (
               Iterator, Option) and then
--           Options.Camera_Lib_Unit_Test_Program_Options.Process_Option (
--             Iterator, Option) and then
--           Options.Nested_Unit_Test_Options.Process_Option (
--             Iterator, Option) and then
             Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (
               Options).Process_Option (Iterator, Option),
             Debug or Trace_Options);
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

