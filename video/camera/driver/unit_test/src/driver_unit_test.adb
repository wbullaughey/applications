with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options;
with Ada_Lib.OS;
--with Ada_lib.Timer;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Command_Name;
with Driver.Unit_Test;

procedure Driver_Unit_Test is

   Options     : constant Driver.Unit_Test.Driver_Unit_Test_Option_Class_Access :=
                  Driver.Unit_Test.Get_Modifiable_Options;
   Debug       : Boolean renames Options.Main_Debug;
   Exit_Code   : Ada_Lib.OS.OS_Exit_Code_Type := Ada_Lib.OS.No_Error;


begin
   if not Driver.Unit_Test.Initialize then
      Put_Line ("Options Initialization failed");
      Ada_Lib.OS.Immediate_Halt (Exit_Code);
   end if;

   if not Options.Process (
         Include_Options      => True,
         Include_Non_Options  => True,
         Modifiers            => Ada_Lib.Help.Modifiers) then
      Put_Line ("Options Process failed");
      Exit_Code := Ada_Lib.OS.Application_Error;
      Ada_Lib.OS.Immediate_Halt (Exit_Code);
   end if;

   Options.Post_Process;
   if Ada_Lib.Help_Test then
      Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
            "failed"
         else
            "completed"));
      if Ada_Lib.Exception_Occured then
         Exit_Code := Ada_Lib.OS.Exception_Exit;
      end if;
   else  -- not Help_test

      Log_In (Debug);
      Put_Line (Command_Name);
      begin
         Driver.Unit_Test.Run_Suite;
         Log_Here (Debug, "returned from run suite");

       exception

         when Fault: others =>
            Trace_Exception (Debug, FAult);
            Exit_Code := Ada_Lib.OS.Exception_Exit;

      end;
   end if;

-- Ada_lib.Timer.Stop;
   Log_Here (Debug, "timer stopped, stop trace tasks");

   Ada_lib.Trace_Tasks.Stop;
   if not Ada_Lib.Trace_Tasks.All_Stopped then
      Ada_Lib.Trace_Tasks.Report;
   end if;
   Log_Out (Debug);
   Ada_Lib.OS.Immediate_Halt (Exit_Code);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Exception_Exit, "exception exit");

end Driver_Unit_Test;
