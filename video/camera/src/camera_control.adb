--with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--with ADA_LIB.GNOGA;
with Ada_Lib.Options.Actual;
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
with GNOGA.Ada_Lib;
with Main;
--with Video.Lib;

procedure Camera_Control is

   Camera_Setup                  : Configuration.Camera.Setup.Setup_Type;
   Connection_Data               : constant Base.Connection_Data_Class_Access :=
                                    new Base.Connection_Data_Type;
   Options                       : aliased Camera.Lib.Options.Program_Options_Type;
   Debug                         : Boolean renames Options.Debug;

begin
   Put_Line (Command_Name);
   Ada_Lib.Options.Actual.Set_Ada_Lib_Program_Options (
      Options'unchecked_access);
   Ada_Lib.Options.Actual.Set_Ada_Lib_Nested_Options (
      Ada_Lib.Options.Actual.Nested_Options_Type (
         Options.Camera_Library)'unchecked_access);
   Ada_Lib.Options.Actual.Set_Ada_Lib_Program_Options (
      Ada_Lib.Options.Actual.Program_Options_Type (
         Options)'unchecked_access);

   if Options.Initialize then
      Log_In (Debug);
      Connection_Data.Initialize;
      GNOGA.Ada_Lib.Set_Connection_Data (
         GNOGA.Ada_Lib.Connection_Data_Class_Access (Connection_Data));
      if Ada_Lib.Help_Test then
         Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
               "failed"
            else
               "completed"));
      else
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
      end if;
      Base.Halt;
   else
      Put_Line ("could not initialize");
   end if;
   Log_Out (Debug);
   ADA_LIB.OS.Immediate_Halt (if Ada_Lib.Exception_Occured then
         Ada_Lib.OS.Exception_Exit
      else
         Ada_Lib.OS.No_Error);

exception
   when Fault: others=>
      Trace_Exception (Fault);
      Base.Halt;

end Camera_Control;

