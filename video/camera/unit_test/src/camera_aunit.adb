with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.Nested;
--with Ada_Lib.Options.Program;
with Ada_Lib.Options.Verification;
with Ada_Lib.OS;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Ada_Lib.Unit_Test;
with Camera.Command_Queue;
with Camera.Lib.Options.Unit_Test;
with Camera.Lib.Unit_Test.Run_Suite;
--with Camera.Unit_Test;
with Command_Name;

-- pragma Elaborate (Ada_Lib.OS);

procedure Camera_AUnit is

   Options        : aliased Camera.Lib.Options.Unit_Test.
                     Camera_Unit_Test_Program_Options_Type (
      Multi_Test  => True);

   Debug    : Boolean renames Camera.Lib.Options.Camera_Options.Main_Debug;
-- Nested_Program_Options
--          : aliased Camera.Lib.Options.
--             Camera_Lib_Options_Nested_Options_Type (Multi_Test => True);
begin
--Debug := True;
--Trace_Tests := True;
   Log_In (Debug);
   Put_Line (Command_Name);
   Ada_Lib.Options.Verification.Set_Ada_Lib_Program_Options (
      Ada_Lib.Options.Verification.Verification_Program_Options_Type'class (
            Options)'unchecked_access,
         Ada_Lib.Options.Verification.Verification_Nested_Options_Type (
            Options.Nested_Options)'unchecked_access);
-- Ada_Lib.Options.Verification.Set_Ada_Lib_Nested_Options (
--    Ada_Lib.Options.Verification.Verification_Nested_Options_Type'class (
--       Nested_Program_Options)'unchecked_access);

   Tag_History (Debug, "options",
      Camera.Lib.Options.Unit_Test.Camera_Unit_Test_Program_Options_Type'class (
         Options)'tag);
   if Options.Initialize then
      Log_Here (Debug);
      if Options.Process (
         Include_Options      => True,
         Include_Non_Options  => False,
         Modifiers            => Ada_Lib.Help.Modifiers) then

         Log_Here (Debug);
         Options.Post_Process;
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
            Camera.Lib.Unit_Test.Run_Suite (Options);
            Log_Here (Debug, "returned from run suite");
            Camera.Command_Queue.Stop_Task;
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
      Put_Line ("could not initialize options");
      Log_Out (Debug);
   end if;

exception

   when Fault: Camera.Lib.Unit_Test.Failed =>
      Options.Display_Help (Ada.Exceptions.Exception_Message (
         Fault), True);

   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera_AUnit;


