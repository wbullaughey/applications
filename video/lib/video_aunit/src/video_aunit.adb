with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Ada_Lib.OS;
with Ada_lib.Timer;
--with Video.Configuration;
with Camera.Lib.Unit_Test;
with Command_Name;
with GNOGA.Application;
with Video.Lib;
with Runtime_Options;

procedure Video_AUnit is

   Debug                         : Boolean := False; -- renames this causes
                                    -- preelaboration to hang
                                    -- Video.Lib.Options.Debug;

begin
   Runtime_Options.Initialize;
   Debug := Runtime_Options.Debug;
   Log_In (Debug);
   Put_Line (Command_Name);
   begin
      GNOGA.Application.Open_URL;
      Ada_lib.Trace_Tasks.Start ("main");

      Log_Here (Debug, "start run suite");
      Camera.Lib.Unit_Test.Run_Suite (Runtime_Options.Options.all);
      Log_Here (Debug, "returned from run suite");

    exception

      when Fault: others =>
         Trace_Message_Exception (Fault, Who, Here);

   end;

   Ada_lib.Timer.Stop;
   Log_Here (Debug, "timer stopped, stop trace tasks");

   Ada_lib.Trace_Tasks.Stop;
   Log_Here (Debug, "timer stopped");
   if not Ada_Lib.Trace_Tasks.All_Stopped then
      Ada_Lib.Trace_Tasks.Report;
   end if;
   Log_Out (Debug);
   Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Video_AUnit;
