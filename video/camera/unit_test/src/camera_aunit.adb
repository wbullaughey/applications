with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Options;
with Ada_Lib.OS;
--with Ada_lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Ada_Lib.Unit_Test;
with Camera.Lib.Unit_Test;
with Camera.Command_Queue;
with Command_Name;

procedure Camera_AUnit is

begin
--Trace_Tests := True;
   Put_Line (Command_Name);
   if Camera.Lib.Unit_Test.Initialize then
      Log_In (Debug);
      declare
         Options              : Camera.Lib.Unit_Test.Unit_Test_Options_Type'class
                                 renames Standard.Camera.Lib.Unit_Test.Options.all;
         Debug                : Boolean renames Options.Main_Debug;

      begin
         Ada_lib.Trace_Tasks.Start ("main");

         Log_Here (Debug, "start run suite");
         Camera.Lib.Unit_Test.Run_Suite (Options);
         Log_Here (Debug, "returned from run suite");
      end;
   else
      Put_Line ("could not initialize options");
   end if;

   Log_Here (Debug, "timer stopped, stop trace tasks");
   Camera.Command_Queue.Stop_Task;

   Ada_lib.Trace_Tasks.Stop;
   Log_Here (Debug, "timer stopped");
   if not Ada_Lib.Trace_Tasks.All_Stopped then
      Ada_Lib.Trace_Tasks.Report;
   end if;
   Log_Out (Debug, "failed " & Ada_Lib.Unit_Test.Did_Fail'img);
   Ada_Lib.OS.Immediate_Halt (if Ada_Lib.Unit_Test.Did_Fail then
      Ada_Lib.OS.Application_Error
   else
      Ada_Lib.OS.No_Error);

exception

   when Fault: Camera.Lib.Unit_Test.Failed =>
      Ada_Lib.Options.Program_Options.Help (Ada.Exceptions.Exception_Message (
         Fault), True);

   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera_AUnit;
