with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Help;
with Ada_Lib.Options;
with Ada_Lib.OS;
--with Ada_lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Ada_Lib.Unit_Test;
--with Camera.Lib.Options;
with Camera.Lib.Unit_Test;
with Camera.Command_Queue;
with Command_Name;

procedure Camera_AUnit is

   Options              : aliased Camera.Lib.Unit_Test.Unit_Test_Program_Options_Type;
   Debug                : Boolean renames Options.Main_Debug;

begin
--Debug := True;
--Trace_Tests := True;
   Put_Line (Command_Name);
   Ada_Lib.Options.Set_Ada_Lib_Options (
      Ada_Lib.Options.Interface_Options_Type (Options)'unchecked_access);
   if Options.Initialize then
      Log_In (Debug);
      if Options.Process (
         Include_Options      => True,
         Include_Non_Options  => False,
         Modifiers            => Ada_Lib.Help.Modifiers) then
         Ada_lib.Trace_Tasks.Start ("main");

         Log_Here (Debug, "start run suite");
         Camera.Lib.Unit_Test.Run_Suite (Options);
         Log_Here (Debug, "returned from run suite");
         Camera.Command_Queue.Stop_Task;
         Ada_lib.Trace_Tasks.Stop;
         Log_Here (Debug, "timer stopped");
         if not Ada_Lib.Trace_Tasks.All_Stopped then
            Ada_Lib.Trace_Tasks.Report;
         end if;

         Ada_Lib.OS.Immediate_Halt (if Ada_Lib.Unit_Test.Did_Fail then
            Ada_Lib.OS.Application_Error
         else
            Ada_Lib.OS.No_Error);
      else
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);
      end if;
   else
      Put_Line ("could not initialize options");
   end if;

exception

   when Fault: Camera.Lib.Unit_Test.Failed =>
      Options.Display_Help (Ada.Exceptions.Exception_Message (
         Fault), True);

   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera_AUnit;
