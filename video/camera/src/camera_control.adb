--with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--with ADA_LIB.GNOGA;
with Ada_Lib.Help;
with Ada_Lib.Options.Flags;
with ADA_LIB.OS;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Ada_Lib.Trace_Tasks;
with Camera.Base;
with Camera.Lib.Options;
with GNOGA_Ada_Lib.Base;
with Command_Name;
with Camera.Main;
with Camera.States;
with Configuration.Camera.Setup;
with Configuration.Camera.State;
with Emulator;

procedure Camera_Control is

-- Camera_Setup                  : Configuration.Camera.Setup.Setup_Type;
-- Connection_Data               : constant Camera.GNOGA_Ada_lib.Connection_Data_Class_Access :=
--                                  Camera.Base.Allocate_Connection_Data;
   Options                       : aliased Camera.Lib.Program.Program_Options_Type;
   Debug                         : Boolean renames Options.Debug;
-- Loaded_Configuration_State    : aliased Configuration.Camera.State.State_Type;

begin
--Debug := true;
   Put_Line (Command_Name);
   Ada_Lib.Options.Flags.Set_Ada_Lib_Program_Options (
      Options'unchecked_access);
   Ada_Lib.Options.Flags.Set_Ada_Lib_Nested_Options (
      Ada_Lib.Options.Nested.Nested_Options_Type (
         Options.Camera_Library)'unchecked_access);

   if Options.Initialize then
      Log_In (Debug, "Help_Test " & Ada_Lib.Options.Ada_Lib_Environment.Help_Test'img);
--    Connection_Data.Initialize;
      if Options.Process (
         Include_Options      => True,
         Include_Non_Options  => False,
         Modifiers            => Ada_Lib.Help.Modifiers) then

         Options.Post_Process;
         if Ada_Lib.Options.Ada_Lib_Environment.Help_Test then
            Put_Line ("help test " & (if Ada_Lib.Exception_Occured then
                  "failed"
               else
                  "completed"));
         else
--          Loaded_Configuration_State.Load (  -- state removed from connection
--             Location => Options.Camera_Library.Location,
--             Name     => Configuration.Camera.State.File_Path);
--          Camera_Setup.Load (Loaded_Configuration_State,
--             Configuration.Camera.Setup.File_Path);
            Camera.State.Load (Options.Location);
            Log_Here (Debug);

            declare
               Configuration_State  : Configuration.Camera.State.State_Type
                  renames Camera.States.
                     Get_Writeable_Configuration_State (
                        Loaded_Configuration_State.Get_Camera_ID).all;
            begin
               Configuration_State.Copy (Loaded_Configuration_State);
               if Options.Camera_Library.Simulate then
                  Emulator.Create;
               end if;

               Ada_Lib.Trace_Tasks.Start ("Main");
               Log_Here (Debug);

               GNOGA_Ada_Lib.Base.Run (
                  Handler              => Camera.Main.On_Connect'Unrestricted_Access,
                  Directory            => Camera.Lib.Options.Current_Directory,
                  Port                 => Options.GNOGA.HTTP_Port,
                  Verbose              => Options.Verbose,
                  Wait_For_Completion  => True);
            end;
            Log_Here (Debug);
         end if;
         Camera.Base.Halt;
      else  -- Options.Process false
         Log_Out (Debug);
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);
      end if;
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
      Ada_Lib.OS.Exception_Halt (Fault, "failed in main");

end Camera_Control;

