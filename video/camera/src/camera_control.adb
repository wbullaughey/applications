--with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with ADA_LIB.GNOGA;
with Ada_Lib.Options;
with ADA_LIB.OS;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
--with Camera.Lib.Base;
with Camera.Lib.Connection;
with Camera.Lib.Options;
with Command_Name;
with Configuration.Camera.Setup;
with Configuration.Camera.State;
--with Configuration.State;
with Emulator;
--with GNOGA.Application;
with Main;
--with Video.Lib;

procedure Camera_Control is

   Camera_Setup                  : Configuration.Camera.Setup.Setup_Type;
   Connection_Data               : aliased Camera.Lib.Connection.
                                    Connection_Data_Type;
   Options                       : aliased Camera.Lib.Options.
                                    Camera_Options_Type;
   Debug                         : Boolean renames Options.Debug;

begin
   Put_Line (Command_Name);
   Ada_Lib.Options.Set_Ada_Lib_Options (Options'unchecked_access);
   if Options.Initialize then
      Log_In (Debug);
      Connection_Data.Initialize;
      Ada_Lib.GNOGA.Set_Connection_Data (Connection_Data'unchecked_access);
      Log_Here (Debug, "location " & Options.Camera_Library.Location'img);

      Connection_Data.State.Load (
         Location => Options.Camera_Library.Location,
         Name     => Configuration.Camera.State.File_Path);

      Camera_Setup.Load (Connection_Data.State,
         Configuration.Camera.Setup.File_Path);
      Log_Here (Debug);

      if Options.Camera_Library.Simulate then
         Emulator.Create;
      end if;

      Ada_Lib.Trace_Tasks.Start ("Main");
      Log_Here (Debug);

      Main.Run (
         Directory            => Camera.Lib.Current_Directory,
         Port                 => Options.GNOGA.HTTP_Port,
         Verbose              => Options.Verbose,
         Wait_For_Completion  => True);

      Log_Here (Debug);
      Camera.Lib.Connection.Halt;
   else
      Put_Line ("could not initialize");
   end if;
   Log_Out (Debug);
   ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
   when Fault: others=>
      Trace_Exception (Fault);
      Camera.Lib.Connection.Halt;

end Camera_Control;

