--with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.Nested;
with Ada_Lib.Options.AUnit_Lib;
with Ada_Lib.Options.Program;
with Ada_Lib.Options.Verification;
with Ada_Lib.OS;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Ada_Lib.Unit_Test;
with Command_Name;
with Video.Lib.Unit_Test;

procedure Video_AUnit is


   Command_Parameters
                  : aliased constant Ada_Lib.Options.Argument_Array := (
                     1 => Ada_Lib.Strings.Unlimited.Coerce (
                        "<camera configuration>"));
-- Options        : aliased Video.Lib.Unit_Test.Program_Options_Type (
--    Multi_Test  => True);
      Aunit_Options  : aliased Video.Lib.Unit_Test.
                        Aunit_Program_Options_Type (
         Multi_Test        => True,
         Options_Selection => Ada_Lib.Options.AUnit_Lib.
                                 Unit_Test_With_No_Database_Or_Template);

   Debug    : Boolean renames Video.Lib.Unit_Test.Options_Package.Main_Debug;
-- Nested_Program_Options
--          : aliased Camera.Lib.Options.
--             Camera_Lib_Options_Nested_Options_Type (Multi_Test => True);
begin
--Debug := True;
--Trace_Tests := True;
   Log_In (Debug);
   Ada_Lib.Options.Program.Set_Command_Parameters (
      Command_Parameters'unchecked_access);
   Put_Line (Command_Name);
   Ada_Lib.Options.Verification.Set_Ada_Lib_Program_Options (
      Ada_Lib.Options.Verification.Verification_Program_Options_Type'class (
            Aunit_Options)'unchecked_access,
            Aunit_Options.Nested_Unit_Test_Options'unchecked_access);
   if Aunit_Options.Initialize then
      Log_Here (Debug);
      if Aunit_Options.Process (
         Include_Options      => True,
         Include_Non_Options  => False,
         Modifiers            => Ada_Lib.Help.Modifiers) then

         Log_Here (Debug);
         Aunit_Options.Post_Process;
         if Ada_Lib.Options.Ada_Lib_Environment.Help_Test then
            Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
                  "failed"
               else
                  "completed"));
            Log_Out (Debug);
            Ada_Lib.OS.Immediate_Halt (if Ada_Lib.Exception_Occured then
               Ada_Lib.OS.Assertion_Exit
            else
               Ada_Lib.OS.No_Error);
         else  -- not Help_test
            Log_Here (Debug, "start run suite");
            Ada_lib.Trace_Tasks.Start ("main");
            Video.Lib.Unit_Test.Run_Suite (Aunit_Options);
            Log_Here (Debug, "returned from run suite");
            Ada_lib.Trace_Tasks.Stop;
            Log_Here (Debug, "timer stopped");
            if not Ada_Lib.Trace_Tasks.All_Stopped then
               Ada_Lib.Trace_Tasks.Report;
            end if;
         end if;

         Log_Out (Debug);
         Ada_Lib.OS.Immediate_Halt (if Ada_Lib.Unit_Test.Did_Fail then
               Ada_Lib.OS.Application_Error
            else
               Ada_Lib.OS.No_Error);
      else  -- Options.Process false
         Log_Out (Debug);
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);
      end if;
   else     -- Initialize failed
      Put_Line ("could not initialize Aunit_Options");
      Log_Out (Debug);
   end if;

exception

   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Video_AUnit;


