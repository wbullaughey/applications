--with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories;
with Ada_Lib.Help;
with Ada_Lib.Options.Actual;
with Ada_Lib.OS;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
--with Ada_lib.Timer;
--with Camera.Configuration;
with Command_Name;
with Driver;
--with GNOGA.Application;
--with Gnoga.Gui.Document;
--with Camera.Lib.Unit_Test;
--with Runtime_Options;

procedure Camera_Driver is

   Protected_Options             : aliased Driver.Program_Options_Type;
   Debug                         : Boolean renames Protected_Options.
                                    Driver_Options.Main_Debug;

begin
   Ada_Lib.Options.Actual.Set_Ada_Lib_Program_Options (
         Protected_Options'unchecked_access);

   Protected_Options.Driver_Options.Camera_Directory.Construct (
      Ada.Directories.Current_Directory & "/..");

   if Protected_Options.Initialize then
      if Protected_Options.Process (
            Include_Options      => True,
            Include_Non_Options  => True,
            Modifiers            => Ada_Lib.Help.Modifiers &
                                       Driver.Option_Modifier) then
         Log_In (Debug, Quote ("Camera_Directory",
            Protected_Options.Driver_Options.Camera_Directory));

         Put_Line (Command_Name);
         if Ada_Lib.Help_Test then
            Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
                  "failed"
               else
                  "completed"));
         else
            Driver.Queue_Tests;
            Driver.Run_Selection;
         end if;
   --    Ada_lib.Timer.Stop;
         Log_Here (Debug, "timer stopped, stop trace tasks");

         Ada_lib.Trace_Tasks.Stop;
         if not Ada_Lib.Trace_Tasks.All_Stopped then
            Ada_Lib.Trace_Tasks.Report;
         end if;
         Log_Out (Debug);
         ADA_LIB.OS.Immediate_Halt (
            if Ada_Lib.Exception_Occured then
               Ada_Lib.OS.Exception_Exit
            else
               Ada_Lib.OS.No_Error);
      else
         ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.Application_Error,
            "Process Options failed");
      end if;
   else
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.Application_Error,
         "Initialize Options failed");
   end if;

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera_Driver;
