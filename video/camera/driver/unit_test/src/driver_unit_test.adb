with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Options;
with Ada_Lib.OS;
--with Ada_lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Command_Name;
with Driver.Unit_Test;

procedure Driver_Unit_Test is

   Options              : aliased Driver.Unit_Test.Driver_Unit_Test_Options_Type;
   Debug                : Boolean renames Options.Driver_Options.Main_Debug;

begin
   Ada_Lib.Options.Set_Ada_Lib_Options (Options'unchecked_access);
   Driver.Set_Options (Options.Driver_Options'unchecked_access);

   Options.Driver_Options.Camera_Directory.Construct ("../../../unit_test");

   if not Options.Initialize then
      Put_Line ("Options Initialization failed");
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);
   end if;

   Log_In (Debug);
   Put_Line (Command_Name);
   begin
      Driver.Unit_Test.Run_Suite;
      Log_Here (Debug, "returned from run suite");

    exception

      when Fault: others =>
         Trace_Message_Exception (Fault, Who, Here);

   end;

-- Log_Here (Debug, (if  Ada_lib.Timer.Cancel then
--       "timer stopped"
--    else
--       "could not stop timer"));

   Ada_lib.Trace_Tasks.Stop;
   if not Ada_Lib.Trace_Tasks.All_Stopped then
      Ada_Lib.Trace_Tasks.Report;
   end if;
   Log_Out (Debug);
   Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit);

end Driver_Unit_Test;
