--with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with ADA_LIB.GNOGA;
--with Ada_Lib.Options_Interface;
with ADA_LIB.OS;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
with Base;
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
   Connection_Data               : constant Base.Connection_Data_Class_Access :=
                                    new Base.Connection_Data_Type;
   Options                       : constant Camera.Lib.Options.Options_Access :=
                                    Camera.Lib.Options.Get_Modifyable_Options;
   Debug                         : Boolean renames Options.Debug;

begin
   Put_Line (Command_Name);
   if Options.Initialize then
      Log_In (Debug);
      Connection_Data.Initialize;
      Ada_Lib.GNOGA.Set_Connection_Data (
         Ada_Lib.GNOGA.Connection_Data_Class_Access (Connection_Data));
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
         Directory            => Camera.Lib.Options.Current_Directory,
         Port                 => Options.GNOGA.HTTP_Port,
         Verbose              => Options.Verbose,
         Wait_For_Completion  => True);

      Log_Here (Debug);
      Base.Halt;
   else
      Put_Line ("could not initialize");
   end if;
   Log_Out (Debug);
   ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

exception
   when Fault: others=>
      Trace_Exception (Fault);
      Base.Halt;

end Camera_Control;

