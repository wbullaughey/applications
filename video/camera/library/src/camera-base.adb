with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Options;
with Ada_Lib.OS;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Commands.PTZ_Optics;
with Camera.Configurations;
with Configuration.Camera.State;
with GNAT.Sockets;

-- pragma Elaborate (Ada_Lib.OS);

package body Camera.Base is

-- use type Camera_ID_Type;

   type Full_Configuration_Type is new Configuration_Type with record
      Camera            : Standard.Camera.Commands.Camera_Class_Access := Null;
      Camera_ID         : Camera_ID_Type;
      Camera_Name       : Ada_Lib.Strings.Unlimited.String_Type;
      Camera_Pan        : Absolute_Type;
      Camera_Pan_Speed  : Property_Type;
      Camera_Tilt       : Absolute_Type;
      Camera_Tilt_Speed : Property_Type;
      Camera_Zoom       : Property_Type;
      Options           : Ada_Lib.Options.Base_Flag_Option_Class_Access :=
                           Null;
   end record;

-- type Full_Configuration_Access
--                      is access Full_Configuration_Type;
-- type Full_Configuration_Class_Access
--                      is access Full_Configuration_Type'class;

-- function Configuration_Equal (
--    Left, Right                : in     Full_Configuration_Access
-- ) return Boolean;

   overriding
   function Get_Camera (
      Configuration      : in     Full_Configuration_Type
   )return Camera.Commands.Camera_Class_Access;

   overriding
   function Get_Camera_ID (
      Configuration      : in     Full_Configuration_Type
   ) return Camera_ID_Type;

   overriding
   function Get_Camera_Name (
      Configuration      : in     Full_Configuration_Type
   ) return String;

   ----------------------------------------------------------------
   procedure Allocate (
      State    : in out Configuration_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      State.Configuration_Setup := new Standard.Configuration.Camera.Setup.Setup_Type;
      State.Configuration_State := new Standard.Configuration.Camera.State.State_Type;
      Log_Out (Debug);
   end Allocate;

   ----------------------------------------------------------------
   procedure Deallocate (
      State    : in out Configuration_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Free (State.Configuration_Setup);
      Free (State.Configuration_State);
      Log_Out (Debug);
   end Deallocate;

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
      Configuration      : in     Configuration_Type
   ) return Configuration.Camera.State.State_Access is
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_State;
   end Get_Configuration_State;

-- ----------------------------------------------------------------
-- function Get_Configuration (
--    Camera_State      : in     Camera_State_Type;
--    Index             : in     Positive
-- ) return Configuration_Access is
-- ----------------------------------------------------------------
--
-- begin
--    return Camera_State.Configurations (Index);
-- end Get_Configuration;

   ----------------------------------------------------------------
   function Get_Configuration_State (
      Configuration      : in     Configuration_Type
   ) return access Standard.Configuration.Camera.State.State_Type'class is
   ----------------------------------------------------------------

   begin
      return State.Configuration_State;
   end Get_Configuration_State;

   ----------------------------------------------------------------
   function Get_Current_Camera_ID
   return Camera_ID_Type is
   ----------------------------------------------------------------

   begin
log_here;
declare
result : constant Camera_ID_Type := Current_Camera_ID;
begin
log_here ("result " & result'img);
      return result;
end;
   end Get_Current_Camera_ID;

 ----------------------------------------------------------------
 function Get_Read_Only_Global_State (
    Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
 ) return Configuration_Constant_Access is
 ----------------------------------------------------------------

 begin
    return Configuration_Constant_Access (Allocate_State);
 end Get_Read_Only_Global_State;

   ----------------------------------------------------------------
   function Has_Camera_State (
      Configuration      : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return State.Camera_State /= Null;
   end Has_Camera_State;

   ----------------------------------------------------------------
   function Has_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Configuration_Setup /= Null,
         Debug or Trace_Pre_Post_Conditions);
   end Has_Configuration_Setup;

   ----------------------------------------------------------------
   function Has_Configuration_State (
      Configuration      : in     Configuration_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Configuration_State /= Null,
         Debug or Trace_Pre_Post_Conditions);
   end Has_Configuration_State;

   ----------------------------------------------------------------
   function Has_Current_Camera_ID
   return Boolean is
   ----------------------------------------------------------------

   begin
Log_Here (Current_Camera_ID.image);
log_here (Current_Camera_ID.Set'img);
      return Current_Camera_ID.Set;
   end Has_Current_Camera_ID;

 ----------------------------------------------------------------
 procedure Load (
    Location    : in     Video.Lib.Location_Type) is
 ----------------------------------------------------------------

    Cameras           : Ada_Lib.Configuration.Configuration_Type;
    Current_Directory : constant String :=
                         Standard.Camera.Lib.Options.Current_Directory;
    File_Name         : constant String := "cameras.cfg";
    Path              : constant String :=
                         (if Current_Directory'length > 0 then
                            Current_Directory & "/"
                         else
                            "") & File_Name;
    State             : constant Configuration_Access := new Configuration_Type;

 begin
    Cameras.Load (Path, Create => False);
    declare
       Number_Cameras : constant Natural :=
                         Cameras.Get_Integer ("number_cameras");
    begin
       for Camera in 1 .. Number_Cameras loop
          declare
             Camera_Number     : constant String :=
                                  Ada_Lib.Strings.Trim (Camera'img);
             State_Name        : constant String := Cameras.Get_String (
                                  "state_" & Camera_Number);
             Setup_Name        : constant String := Cameras.Get_String (
                                  "setup_" & Camera_Number);
          begin
             Load (State.all, Setup_Name, State_Name);
          end;
       end loop;
    end;
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

   ---------------------------------------------------------------
   function Allocate_Configuration
   return Configuration_Class_Access is
   ---------------------------------------------------------------

   begin
      return Configuration_Class_Access'(new Full_Configuration_Type);
   end Allocate_Configuration;

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
   overriding
   function Get_Camera (
      Configuration      : in     Full_Configuration_Type
   )return Camera.Commands.Camera_Class_Access is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera;
   end Get_Camera;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_ID (
      Configuration      : in     Full_Configuration_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_Name (
      Configuration      : in     Full_Configuration_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera_Name.Coerce;
   end Get_Camera_Name;

   ----------------------------------------------------------------
   overriding
   function Get_Configuration_Pan_Speed (
      Configuration      : in     Full_Configuration_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Full_Configuration_Type'class (Configuration).Camera_Pan_Speed;
   end Get_Configuration_Pan_Speed;

   ----------------------------------------------------------------
   overriding
   function Get_Configuration_Tilt_Speed (
      Configuration      : in     Full_Configuration_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Full_Configuration_Type'class (Configuration).Camera_Pan_Speed;
   end Get_Configuration_Tilt_Speed;

-- ----------------------------------------------------------------
-- function Get_Configuration (
--    Configuration      : in     Configuration_Type
-- ) return Configuration_Class_Access is
-- ----------------------------------------------------------------
--
-- begin
--    return Configuration_Class_Access (Full_Configuration'access);
-- end Get_Configuration;

   ----------------------------------------------------------------
   function Get_Configuration (
      Configurations    : in     Configurations_Type;
      Index             : in     Positive
   ) return Configuration_Class_Access is
   ----------------------------------------------------------------

   begin
      return Configurations.Configurations (Index)'unchecked_access;
   end Get_Configuration;

   ----------------------------------------------------------------
   function Get_Configuration_State (
      Configuration        : in     Configuration_Type
   ) return access Standard.Configuration.Camera.State.State_Type;
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_State;
   end Get_Configuration_State;

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
      Configurations    : in out Configurations_Class_Access;
      Path              : in     String) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("path", Path));
      Configurations := new Configuration_Type;

      declare
         Config            : Ada_Lib.Configuration.Configuration_Type;

      begin
         Config.Load (Path, False);
         Configurations.Number_Configurations := Natural (Config.Get_Integer (
            "number configurations"));
         for Configuration_Index in Configuration.Number_Configurations loop
            declare
               Configuration  : Configuration_Class_Access renames
                                 Configuratiions.Configurations (Configuration_Index);
               Configuration_Path   : constant String := Path & "." &
                                       Trim (Configuration_Index'img);
            begin
               Configuration.Configuration_Setup := new Configuration.Camera.Setup.Setup_Type;
               Configuration.Configuration_Setup.Load (Configuration_Path);
               Configuration.Configuration_State := new Configuration.Camera.State.State_Type;
               Configuration.Configuration_State.Load (Configuration_Path);
            end;
         end loop;
      end;
      Log_Out (Debug);
   end Load;

   ---------------------------------------------------------------
   procedure Load_Setup (
      Configuration      :    out Configuration_Type;
      Path              : in     String) is
   ---------------------------------------------------------------

   begin
      Configuration.Configuration_Setup.Load (Path);
not_implemented;
   end Load_Setup;

   ---------------------------------------------------------------
   procedure Load_State (
      Configuration      :    out Configuration_Type;
      Path              : in     String) is
   ---------------------------------------------------------------

   begin
      Configuration.Configuration_State.Load (Path);
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

   ---------------------------------------------------------------
   procedure Open_Camera (
      Camera         : in out Configuration_Type;
      Description    : in     Ada_Lib.Strings.String_Constant_Access) is
   ---------------------------------------------------------------

      State_Pointer     : constant Configuration.Camera.State.
                           State_Constant_Access := Standard.Camera.
                              Configurations.Get_Read_Only_Configuration_State;
      State             : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
      Port_Number       : constant Standard.Camera.Port_Type :=
                           State.Get_Host_Port;
      Camera_Address    : constant Ada_Lib.Socket_IO.Address_Type :=
                           State.Get_Host_Address;
      Full_Configuration : Full_Configuration_Type renames
                           Full_Configuration_Type (
                              Configuration_Type'class (Camera));

   begin
      Log_In (Debug,
         Quote (" Camera_URL", Camera_Address.Image) &
         " port" & Port_Number'img);

      Full_Configuration.Camera :=
         Standard.Camera.Commands.Camera_Class_Access'(
            new Standard.Camera.Commands.PTZ_Optics.PTZ_Optics_Type (
               Description));

      Full_Configuration.Camera.Open (Camera_Address, Port_Number);
      Log_Out (Debug);

   exception

      when Fault: GNAT.Sockets.Host_Error =>
         Trace_Exception (Debug, Fault, Here);
         Put_Line ("Could not open camera. Error " &
            Ada.Exceptions.Exception_Message (Fault));
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.Application_Error);

   end Open_Camera;

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

   ----------------------------------------------------------------
   procedure Set_Configuration_Setup (
      Configuration        : in out Configuration_Type;
      Configuration_Setup  : in     Standard.Configuration.Camera.Setup.Setup_Access) is
   ----------------------------------------------------------------

   begin
      Configuration.Configuration_Setup := Configuration_Setup;
   end Set_Configuration_Setup;

   ----------------------------------------------------------------
   procedure Set_Configuration_State (
      Configuration        : in out Configuration_Type;
      Configuration_State  : in     Standard.Configuration.Camera.State.State_Access) is
   ----------------------------------------------------------------

   begin
     Configuration.Configuration_State := Configuration_State;
   end Set_Configuration_State;

-- ---------------------------------------------------------------
-- procedure Set_Main_Window_Connection_ID (
--    Window_ID                  : in     Gnoga.Types.Connection_ID) is
-- ---------------------------------------------------------------
--
-- begin
--    Main_Window_Connection_ID := Window_ID;
-- end Set_Main_Window_Connection_ID;

   ----------------------------------------------------------------
   overriding
   procedure Set_Mouse_Action (
      Configuration      : in     Full_Configuration_Type;
      Action            : in     Mouse_Click_Action_Type) is
   ----------------------------------------------------------------

   begin
not_implemented;
   end Set_Mouse_Action;

begin
--Debug := True;
-- Include_Task := True;
   Log_Here (Elaborate or Trace_Options);
end Camera.Base;
