--with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Configuration;
with Ada_Lib.Options;
with Ada_Lib.OS;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Commands.PTZ_Optics;
--with Camera.Configurations;
with Camera.Lib.Options;
with Configuration.Camera.Setup;
with Configuration.Camera.State;
--with GNAT.Sockets;

-- pragma Elaborate (Ada_Lib.OS);

package body Camera.Base is

   use Ada_Lib.Strings;
-- use type Camera_ID_Type;

-- type Full_Configuration_Type is new Configuration_Type with record
--    Camera            : Standard.Camera.Commands.Camera_Class_Access := Null;
--    Camera_ID         : Camera_ID_Type;
--    Camera_Name       : Ada_Lib.Strings.Unlimited.String_Type;
--    Camera_Pan        : Absolute_Type;
--    Camera_Pan_Speed  : Property_Type;
--    Camera_Tilt       : Absolute_Type;
--    Camera_Tilt_Speed : Property_Type;
--    Camera_Zoom       : Property_Type;
--    Options           : Ada_Lib.Options.Base_Flag_Option_Class_Access :=
--                         Null;
-- end record;
--
-- type Full_Configuration_Access
--                      is access Full_Configuration_Type;
-- type Full_Configuration_Class_Access
--                      is access Full_Configuration_Type'class;

-- function Configuration_Equal (
--    Left, Right                : in     Full_Configuration_Access
-- ) return Boolean;

-- overriding
-- function Get_Camera (
--    Configuration      : in     Full_Configuration_Type
-- )return Camera.Commands.Camera_Class_Access;
--
-- overriding
-- function Get_Camera_ID (
--    Configuration      : in     Full_Configuration_Type
-- ) return Camera_ID_Type;
--
-- overriding
-- function Get_Camera_Name (
--    Configuration      : in     Full_Configuration_Type
-- ) return String;
--
-- overriding
-- procedure Set_Mouse_Action (
--    Configuration      : in     Full_Configuration_Type;
--    Action            : in     Mouse_Click_Action_Type);
--
   Debug    : Boolean renames Lib.Options.Camera_Options.Base_Debug;

-- ----------------------------------------------------------------
-- function Allocate
-- return Configuration_Access is
-- ----------------------------------------------------------------
--
--    Result   : Configuration_Access := new Configuration_Type;
--
-- begin
--    Log_In (Debug);
--    Result.Configuration_Setup :=
--       new Standard.Configuration.Camera.Setup.Setup_Type;
--    Result.Configuration_State :=
--       new Standard.Configuration.Camera.State.State_Type;
--    return Result;
--    Log_Out (Debug);
-- end Allocate;

   ----------------------------------------------------------------
   procedure Deallocate (
      Configuration     : in     Configuration_Access) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
not_implemented;
--    Free (Configuration.Configuration_Setup);
--    Free (Configuration.Configuration_State);
      Log_Out (Debug);
   end Deallocate;

   ----------------------------------------------------------------
   function Get_Configuration (
      Configurations    : in     Configurations_Type;
      Index             : in     Positive
   ) return Configuration_Access is
   ----------------------------------------------------------------

   begin
      return Configurations.Configurations (Index);
   end Get_Configuration;

   ----------------------------------------------------------------
   function Get_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return access Standard.Configuration.Camera.Setup.Setup_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_Setup;
   end Get_Configuration_Setup;

   ----------------------------------------------------------------
   function Get_Configuration_State (
      Configuration        : in     Configuration_Type
   ) return access Standard.Configuration.Camera.State.State_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_State;
   end Get_Configuration_State;

--   ----------------------------------------------------------------
--   function Get_Current_Camera_ID
--   return Camera_ID_Type is
--   ----------------------------------------------------------------
--
--   begin
--log_here;
--declare
--result : constant Camera_ID_Type := Current_Camera_ID;
--begin
--log_here ("result " & result'img);
--      return result;
--end;
--   end Get_Current_Camera_ID;

 ----------------------------------------------------------------
   function Get_Number_Configurations (
      Configurations         : in     Configurations_Type
   ) return Natural is
 ----------------------------------------------------------------

   begin
not_implemented;
return 0;
   end Get_Number_Configurations;

   ----------------------------------------------------------------
   function Get_Read_Only_Configuration (
      Configurations         : in     Configurations_Type
   ) return Camera_Ready_Only_State_Class_Access is
   ----------------------------------------------------------------

   begin
not_implemented;
return null;
   end Get_Read_Only_Configuration;

------------------------------------------------------------------
--function Get_Read_Only_Global_State (
--   Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
--) return Configuration_Constant_Access is
------------------------------------------------------------------
--
--begin
--   return Configuration_Constant_Access (Allocate_State);
--end Get_Read_Only_Global_State;

-- ----------------------------------------------------------------
-- function Has_Camera_State (
--    Configuration      : in     Configuration_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Configuration.Camera_State /= Null;
-- end Has_Camera_State;

   ----------------------------------------------------------------
   function Has_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Configuration.Configuration_Setup /= Null,
         Debug or Trace_Pre_Post_Conditions);
   end Has_Configuration_Setup;

   ----------------------------------------------------------------
   function Has_Configuration (
      Configuration      : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Configuration.Configuration_State /= Null,
         Debug or Trace_Pre_Post_Conditions);
   end Has_Configuration;

--   ----------------------------------------------------------------
--   function Has_Current_Camera_ID
--   return Boolean is
--   ----------------------------------------------------------------
--
--   begin
--Log_Here (Current_Camera_ID.image);
--log_here (Current_Camera_ID.Set'img);
--      return Current_Camera_ID.Set;
--   end Has_Current_Camera_ID;

 ----------------------------------------------------------------
   function Have_Video_Address (
      Configuration      : in     Configuration_Type
   ) return Boolean is
 ----------------------------------------------------------------

   begin
not_implemented;
      return False;
   end Have_Video_Address;

 ----------------------------------------------------------------
 procedure Load (
    Location    : in     Video.Lib.Location_Type) is
 ----------------------------------------------------------------

    Camera_Configurations  : Configurations_Type;
    Configuration_File     : Ada_Lib.Configuration.Configuration_Type;
    Current_Directory      : constant String :=
                              Standard.Camera.Lib.Options.Current_Directory;
    File_Name              : constant String := "cameras.cfg";
    Path                   : constant String :=
                              (if Current_Directory'length > 0 then
                                 Current_Directory & "/"
                              else
                                 "") & File_Name;
   begin
      Log_In (Debug, Quote ("Current_Directory", Current_Directory) &
         Quote (" File_Name", File_Name) &
         Quote (" path", Path));
      Configuration_File.Load (Path, Create => False);
      Camera_Configurations.Number_Configurations :=
         Configuration_File.Get_Integer ("number_cameras");

      for Camera in 1 .. Camera_Configurations.Number_Configurations loop
         declare
            Camera_Configuration_File
                           : Ada_Lib.Configuration.Configuration_Type;
            Configuration  : Configuration_Access renames
                              Camera_Configurations.Configurations (Camera);
            Camera_File_Name
                           : constant String := "camera_" &
                               Ada_Lib.Strings.Trim (Camera'img);
         begin
            Configuration := new Configuration_Type;
            Camera_Configuration_File.Load (Camera_File_Name,
               Create => False);
            declare
               Configuration_File
                           : Ada_Lib.Configuration.Configuration_Type;
               State_Name  : constant String :=
                              Camera_Configuration_File.Get_String (
                                 "state_" & Trim (Camera'img));
               Setup_Name  : constant String :=
                              Camera_Configuration_File.Get_String (
                                 "setup_" & Trim (Camera'img));
            begin
               Configuration.Configuration_State.Load (
                  Configuration_File, Location, State_Name);
               Configuration.Configuration_Setup.Load (
                  Configuration.Configuration_State.all, Setup_Name);
            end;
         end;
      end loop;
   end Load;

-- overriding
-- function Get_Configuration_Pan_Speed (
--    Configuration      : in     Full_Configuration_Type
-- ) return Data_Type;
--
-- overriding
-- function Get_Configuration_Tilt_Speed (
--    Configuration      : in     Full_Configuration_Type
-- ) return Data_Type;
--
-- overriding
-- procedure Set_Mouse_Action (
--    Configuration      : in     Full_Configuration_Type;
--    Action            : in     Camera.Mouse_Click_Action_Type);

-- ---------------------------------------------------------------
-- function Allocate_Configuration
-- return Configuration_Class_Access is
-- ---------------------------------------------------------------
--
-- begin
--    return Configuration_Class_Access'(new Configuration_Type);
-- end Allocate_Configuration;

--   ---------------------------------------------------------------
--   procedure Allocate_Connection_Data is
--   ---------------------------------------------------------------
--
--      Base_Data   : constant Base.Base_Data_Access :=
--                           Allocate_Connection_Data;
--      pragma Unreferenced (Base_Data);
--
--   begin
--      Log_Here (Debug);
--   end Allocate_Connection_Data;
--
--   ---------------------------------------------------------------
--   function Allocate_Connection_Data
--   return Base_Data_Access is
--   ---------------------------------------------------------------
--
--      Base_Data            : constant Base.Base_Data_Access :=
--                                    new Base.Base_Data_Type;
--   begin
--      Log_Here (Debug);
----    Base_Data.Get_Connection_Data.Main_Data := new Main.Window_Connection_Type;
--      GNOGA_Ada_Lib.Set_Connection_Data (
--         Ada_Lib.GNOGA.Connection_Data_Class_Access (Base_Data));
--      return Base_Data;
--   end Allocate_Connection_Data;

-- ----------------------------------------------------------------
-- function Configuration_Equal (
--    Left, Right                : in     Full_Configuration_Access
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Left = Right;
-- end Configuration_Equal;

   ----------------------------------------------------------------
   function Get_Camera (
      Configuration      : in     Configuration_Type
   )return Camera.Commands.Camera_Class_Access is
   ----------------------------------------------------------------

   begin
not_implemented;
--    return Configuration.Camera;
return null;
   end Get_Camera;

   ----------------------------------------------------------------
   function Get_Camera_ID (
      Configuration      : in     Configuration_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------
   function Get_Camera_Name (
      Configuration      : in     Configuration_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera_Name.Coerce;
   end Get_Camera_Name;

   ----------------------------------------------------------------
   function Get_Configuration_Pan_Speed (
      Configuration      : in     Configuration_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Configuration_Type'class (Configuration
         ).Default_Camera_Pan_Speed;
   end Get_Configuration_Pan_Speed;

   ----------------------------------------------------------------
   function Get_Configuration_Tilt_Speed (
      Configuration      : in     Configuration_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Configuration_Type'class (Configuration
         ).Default_Camera_Tilt_Speed;
   end Get_Configuration_Tilt_Speed;

--   ----------------------------------------------------------------
--   -- gets connection data for current active window
--   function Get_Connection_Data
--   return Window_Connection_Access is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
--   end Get_Connection_Data;

--   ----------------------------------------------------------------
--   -- gets connection data for specified window
--   function Get_Connection_Data (
--      Window_ID                  : in     Window_ID_Type
--   ) return Window_Connection_Access is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
--   end Get_Connection_Data;

--   ----------------------------------------------------------------
--   function Get_Connection_Data (
--      Base_Data            : in     Gnoga.Types.Base_Data_Type'class
--   ) return Base_Data_Access is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
--   end Get_Connection_Data;
--
--   ----------------------------------------------------------------
--   function Get_Connection_Data (
--      Base_Data            : in     Base_Data_Type
--   ) return Base_Data_Access is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
--   end Get_Connection_Data;
--
--   ----------------------------------------------------------------
--   function Get_Connection_Data (
--      Base_Data            : in     Base_Data_Type;
--      Window_ID                  : in     Window_ID_Type
--   ) return Base_Data_Access is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
--   end Get_Connection_Data;

   ----------------------------------------------------------------
   function Get_Location (
      Configuration     : in     Configuration_Type
   ) return Video.Lib.Location_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Location;
   end Get_Location;

   ----------------------------------------------------------------
   function Get_Setup_Path (
      Configuration     : in     Configuration_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Configuration.Setup_Path.Coerce;
   end Get_Setup_Path;

   ----------------------------------------------------------------
   function Get_Simulate (
      Configuration     : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Configuration.Simulate;
   end Get_Simulate;

   ----------------------------------------------------------------
   function Get_State_Path (
      Configuration     : in     Configuration_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Configuration.State_Path.Coerce;
   end Get_State_Path;

   ----------------------------------------------------------------
   procedure Halt is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);
   end Halt;

--   ---------------------------------------------------------------
--   function Has_Connection_Data
--   return Boolean is
--   ---------------------------------------------------------------
--
--   begin
--not_implemented;
--return false;
----    return Has_Main_Window_Connection_ID and then
--   end Has_Connection_Data;

--   ------------------------------------------------------------------------------------------------
--   procedure Initialize_GNOGA (
--      Handler                    : in     Gnoga.Application.Multi_Connect.Application_Connect_Event;
--      Application_Title          : in     String;
--      Port                       : in     Ada_Lib.Socket_IO.Port_Type;
--      Wait_For_Message_Loop_Exit        : in     Boolean;
--      Handler_Path               : in     String := "default";
--      Verbose                    : in     Boolean := False) is
--   ---------------------------------------------------------------
--
--   begin
--      Log_In (Debug, "GNOGA_Initialized " & GNOGA_Initialized'img &
--         " Wait_For_Message_Loop_Exit " & Wait_For_Message_Loop_Exit'img &
--         " port" & Port'img &
--         " verbose " & Verbose'img);
--
--      if not GNOGA_Initialized then
--         GNOGA_Initialized := True;
--
--         GNOGA.Application.Title (Application_Title);
--         GNOGA.Application.HTML_On_Close
--           ("<b>Connection to Application has been terminated</b>");
--
--         Gnoga.Application.Multi_Connect.Initialize (
----          Event=> Handler,
--            Port => Integer (Port),
--            Boot => "boot_jqueryui.html",
--            Verbose => Verbose);
--      end if;
--
--
--      Gnoga.Application.Multi_Connect.On_Connect_Handler
--        (Event => Handler,
--         Path  => Handler_Path);
--
--
--      if Message_Loop = Null then      -- only one per program
--         Log_Here (Debug);
--         Message_Loop := new Message_Loop_Task;
--
--         while Task_ID = Ada.Task_Identification.Null_Task_Id loop
----          Log (Debug, Here, Who & " wait for task to initialize");
--            delay 0.1;  -- let task initialize
--            Task_ID := Ada.Task_Identification.Current_Task;
--         end loop;
--         Log_Here (Debug);
--         delay 0.1;        -- let message loop initialize
--      end if;
----    Log_Here (Debug, "Main_Created " & Main_Created'img);
----
----    while not Main_Created loop     -- wait for On_Connect to complete
----       delay 0.1;
----    end loop;
--
--      Log_Here (Debug, "Wait_For_Message_Loop_Exit " & Wait_For_Message_Loop_Exit'img);
--
--      if Wait_For_Message_Loop_Exit then
--         Message_Loop_Signal.Wait;
--      end if;
--      Log_Out (Debug);
--
--   exception
--      when Fault: others =>
--         Log_Exception (Debug);
--         Trace_Exception (Debug, Fault);
--         raise Failed with "could not Initialize_GNOGA";
--   end Initialize_GNOGA;

   ---------------------------------------------------------------
   procedure Load (
      Configurations       : in out Configurations_Type;
      Path                 : in     String) is
   ---------------------------------------------------------------

         Config            : Ada_Lib.Configuration.Configuration_Type;

   begin
      Log_In (Debug, Quote ("path", Path));
      Config.Load (Path, False);

      declare
         Number_Cameras    : constant Natural :=
                              Natural (Config.Get_Integer ("number_cameras"));
      begin
         Configurations.Configurations := new Configuration_Array (
                                             1 .. Number_Cameras);

         for Index in 1 .. Configurations.Number_Configurations loop
            Configurations.Configurations (Index).Load (Path, Index);
--             Config.Get_String ("setup_" & Trim (Index'img)),
--             Config.Get_String ("state_" & Trim (Index'img)));
         end loop;
      end;
   end Load;

   ---------------------------------------------------------------
   procedure Load (
      Configuration        : in out Configuration_Type;
      Path                 : in     String;
      Camera_Index         : in     Positive) is
   ---------------------------------------------------------------

      Camera_Suffix        : constant String := Trim (Camera_Index'img);
      Configuration_File   : Ada_Lib.Configuration.Configuration_Type;

      ------------------------------------------------------------
      function Path_Type (
         Camera_Index      : in     Positive;
         Prefix            : in     String;
         Root              : in     String
      ) return String is
      ------------------------------------------------------------

         Parameter         : constant String :=
                              Prefix & Camera_Suffix;

--                            (if Prefix'length = 0 then
--                                  Root
--                               else
--                                  Prefix & "_" & Root &
--                               Camera_Suffix);
      begin
         Log_In (Debug,
            "Camera_Index" & Camera_Index'img &
            Quote (" prefix", Prefix) &
            Quote (" root", Root) &
            Quote (" parameter", Parameter));

         if Configuration_File.Has (Parameter) then
            declare
               Result         : constant String :=
                                 Configuration_File.Get_String (Parameter);
            begin
               Log_Out (Debug, Quote ("Result", Result));
               return Result;
            end;
         else
            declare
               Result         : constant String :=
                                 Prefix & Trim (Camera_Index'img);
            begin
               Log_Out (Debug, Quote ("Result", Result));
               return Result;
            end;
         end if;
      end Path_Type;
      ------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("path", Path) & " camera index" &
         Camera_Index'img);
      Configuration_File.Load (Path, Create => False);
      Configuration.Setup_Path.Construct (Path_Type (
         Camera_Index, "camera_setup_", "setup"));
      Configuration.Setup_Path.Construct (Path_Type (
         Camera_Index, "camera_state_", "state"));
      Configuration.Configuration_Setup :=
         new Standard.Configuration.Camera.Setup.Setup_Type;
      Configuration.Configuration_State :=
         new Standard.Configuration.Camera.State.State_Type;

      declare
         State_Configuration_File
                        : Ada_Lib.Configuration.Configuration_Type;
      begin
         Configuration.Configuration_State.Load (State_Configuration_File,
            Configuration.Location, Configuration.Setup_Path.Coerce);
      end;
      Configuration.Configuration_Setup.Load (
         Configuration.Configuration_State.all,
            Configuration.Setup_Path.Coerce);
      Log_Out (Debug, Quote ("setup path", Configuration.Setup_Path) &
                     Quote (" state path", Configuration.State_Path));
   end Load;

   ---------------------------------------------------------------
   procedure Load_Setup (
      Configuration        : in out Configuration_Type;
      Path                 : in     String) is
   ---------------------------------------------------------------

   begin
--    Configuration.Configuration_Setup.Load (Path);
not_implemented;
   end Load_Setup;

   ---------------------------------------------------------------
   procedure Load_State (
      Configuration        : in out Configuration_Type;
      Path                 : in     String) is
   ---------------------------------------------------------------

   begin
--    Configuration.Configuration_State.Load (Path);
not_implemented;
   end Load_State;

--   ----------------------------------------------------------------
--   function New_Base_Data (
--      Window_ID                  : in     Window_ID_Type
--   ) return Base_Data_Access is
--   ----------------------------------------------------------------
--
--   begin
--      Main_Window_Connection_ID := Window_ID;
--not_implemented;
--return null;
--   end New_Base_Data;

   ----------------------------------------------------------------
   procedure Report_Exception (
      Window                     : in out Gnoga.Gui.Window.Window_Type'class;
      Fault                      : in     Ada.Exceptions.Exception_Occurrence;
      Message                    : in     String;
      Where                      : in     String := GNAT.Source_Info.Source_Location) is
   ----------------------------------------------------------------

      Error_Message              : constant String :=
                                    Ada.Exceptions.Exception_Message (Fault) &
                                    ". " & Message & (if Debug then
                                          " raised at " & Where
                                       else
                                          "");
   begin
      Window.Alert (Error_Message);
   end Report_Exception;

-- ----------------------------------------------------------------
-- procedure Set_Configuration_Setup (
--    Configuration        : in out Configuration_Type;
--    Configuration_Setup  : in     Standard.Configuration.Camera.
--                                     Setup.Setup_Access) is
-- ----------------------------------------------------------------
--
-- begin
--    Configuration.Configuration_Setup := Configuration_Setup;
-- end Set_Configuration_Setup;
--
-- ----------------------------------------------------------------
-- procedure Set_Configuration_State (
--    Configuration        : in out Configuration_Type;
--    Configuration_State  : in     Standard.Configuration.Camera.State.State_Access) is
-- ----------------------------------------------------------------
--
-- begin
--   Configuration.Configuration_State := Configuration_State;
-- end Set_Configuration_State;

-- ---------------------------------------------------------------
-- procedure Set_Main_Window_Connection_ID (
--    Window_ID                  : in     Gnoga.Types.Connection_ID) is
-- ---------------------------------------------------------------
--
-- begin
--    Main_Window_Connection_ID := Window_ID;
-- end Set_Main_Window_Connection_ID;

--   ----------------------------------------------------------------
--   procedure Set_Mouse_Action (
--      Configuration      : in     Configuration_Type;
--      Action            : in     Mouse_Click_Action_Type) is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--   end Set_Mouse_Action;

begin
--Debug := True;
-- Include_Task := True;
   Log_Here (Elaborate or Trace_Options);
end Camera.Base;
