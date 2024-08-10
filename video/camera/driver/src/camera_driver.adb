with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Options;
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

   Options                       : aliased Driver.Program_Options_Type;
   Debug                         : Boolean renames Options.Driver_Options.Main_Debug;

begin
   Ada_Lib.Options.Set_Ada_Lib_Options (
      Ada_Lib.Options.Interface_Options_Class_Access'(
         Options'unchecked_access));

   Options.Driver_Options.Camera_Directory.Construct (
      Ada.Directories.Current_Directory);

   if Options.Initialize and then
         Options.Process (
            Include_Options      => True,
            Include_Non_Options  => True) then
      Log_In (Debug);
      Put_Line (Command_Name);
      Driver.Queue_Tests;
      Driver.Run_Selection;
--    Ada_lib.Timer.Stop;
      Log_Here (Debug, "timer stopped, stop trace tasks");

      Ada_lib.Trace_Tasks.Stop;
      if not Ada_Lib.Trace_Tasks.All_Stopped then
         Ada_Lib.Trace_Tasks.Report;
      end if;
      Log_Out (Debug);
   else
      Ada_Lib.Options.Get_Ada_Lib_Read_Only_Options.Display_Help ("Initialize Options failed", False);
   end if;

   Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera_Driver;
