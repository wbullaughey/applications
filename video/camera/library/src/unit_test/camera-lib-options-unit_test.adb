with Ada_Lib.OS;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Camera.Lib.Options.Unit_Test is

   Debug       : Boolean renames Camera_Lib_Unit_Test.Unit_Test_Debug;
   Initialize_Recursed
               : Boolean := False;

   ----------------------------------------------------------------------------
   overriding
   procedure Display_Help (
                              -- prints full help, aborts program
     Options   : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
     Message   : in     String := "";   -- leave blank no error help
     Halt      : in     Boolean := True) is
   ----------------------------------------------------------------------------

   begin
not_implemented;
   end Display_Help;

   ----------------------------------------------------------------------------
   function Get_Modifiable_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Class_Access is
   ----------------------------------------------------------------------------

   begin
not_implemented;
return null;
   end Get_Modifiable_Camera_Unit_Test_Options;

   ----------------------------------------------------------------------------
   function Get_Read_Only_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Constant_Class_Access is
   ----------------------------------------------------------------------------

   begin
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
             Options.Camera_Lib_Nested_Options.Initialize (From) and
             Options.Camera_Lib_Unit_Test_Program_Options.Initialize (From) and
             Options.Nested_Unit_Test_Options.Initialize (From),
             Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   function Process (     -- process command line options
     Options   : in out Camera_Unit_Test_Program_Options_Type;
     Iterator  : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
not_implemented;
return false;
   end Process;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options     : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode   : in     Ada_Lib.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
not_implemented;
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
not_implemented;
return false;
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
   ----------------------------------------------------------------------------

   begin
not_implemented;
   end Trace_Parse;

begin
Trace_Options := True;
   Log_Here (Debug or Elaborate or Trace_Options);


exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera.Lib.Options.Unit_Test;

