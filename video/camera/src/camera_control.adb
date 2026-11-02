--with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--with ADA_LIB.GNOGA;
with Ada_Lib.Help;
--with Ada_Lib.Options.Flags;
--with Ada_Lib.Options.Nested;
with Ada_Lib.Options.Verification;
with ADA_LIB.OS;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Trace_Tasks;
--with Camera.Base;
with Camera.Lib.Options;
--with Camera.Configuration;
with GNOGA_Ada_Lib.Base;
with Command_Name;
with Camera.Lib.Base;
with Camera.Main;
with Camera.Preset;
--with Camera.Configuration;
--with Camera.Configurations;
with Configuration.Setup;
--with Configuration.State;
--with Video.Lib;
--with Video.Object.Setup;
--with Video.Object.Camera.State;
--with Video.Object.State;
--with Emulator;

procedure Camera_Control is

   Debug             : Boolean renames Camera.Lib.Options.Camera_Options.
                        Camera_Control_Debug;
   Options           : aliased Camera.Lib.Options.Program_Options_Type (
                        Multi_Test => False);
-- Standard_Presets  : array (Camera.Preset.Which_Preset_Type) of
--                      Video.Lib.Preset_ID_Type := (
--                         others => Video.Lib.Preset_ID_Not_Set);

--   ----------------------------------------------------------------------------
--   procedure Load_Configuration (
--      Configratuion  : in     Camera.Base.Configuration_Type'class) is
--   ----------------------------------------------------------------------------
--
--   begin
--      Configratuion.Load (Path);
--
--      declare
--         Configuration_State  : Configuration.State.State_Type
--            renames Camera.Configurations.
--               Get_Configuration_State (
--                  Camera_State.Get_Camera_ID).all;
--      begin
----       Camera_State.Copy (Configuration_State);
--         if Configratuion.Get_Simulate then
--            Emulator.Create;
--         end if;
--
--         Ada_Lib.Trace_Tasks.Start ("Main");
--         Log_Here (Debug);
--
--         GNOGA_Ada_Lib.Base.Run (
--            Handler     => Camera.Main.On_Connect'Unrestricted_Access,
--            Directory   => Camera.Lib.Options.Current_Directory,
--            Port        => Options.Library_Options.GNOGA_Options.HTTP_Port,
--            Verbose     => Options.Verbose,
--            Wait_For_Message_Loop_Exit  => True);
--      end;
--   end Load_Configuration;
   ----------------------------------------------------------------------------

begin
--Debug := true;
   Put_Line (Command_Name);
   Ada_Lib.Options.Verification.Set_Ada_Lib_Program_Options (
      Options'unchecked_access,
      Ada_Lib.Options.Verification.Verification_Nested_Options_Type (
         Options.Camera_Lib_Options_Nested_Options)'unchecked_access);

   if Options.Initialize then
      Log_In (Debug, "Help_Test " & Ada_Lib.Options.Ada_Lib_Environment.Help_Test'img);
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
            declare
               Path  : constant String := Options.Camera_State_Path.Coerce;

            begin
               Log_Here (Debug, Quote ("path", Path));
               Configuration.Setup.Load (Path);
               Camera.Preset.Allocate_Presets (
                  Configuration.Setup.Get_Number_Presets);
               GNOGA_Ada_Lib.Base.Run (
                 Handler     => Camera.Main.On_Connect'Unrestricted_Access,
                 Directory   => Camera.Lib.Options.Current_Directory,
                 Port        => Options.Camera_Lib_Options_Nested_Options.
                                 GNOGA_Ada_Lib_Option.Get_HTTP_Port,
                 Verbose     => Options.Camera_Lib_Options_Nested_Options.Verbose,
                 Wait_For_Message_Loop_Exit  => True);
               Camera.Lib.Base.Halt;
            end;
         end if;
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

