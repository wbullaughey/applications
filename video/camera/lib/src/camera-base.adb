with Ada.Text_IO; use  Ada.Text_IO;
with Ada_Lib.Options;
with Ada_Lib.OS;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Commands.PTZ_Optics;
with Camera.States;
with Configuration.Camera.State;
with GNAT.Sockets;

package body Camera.Base is

-- use type Camera_ID_Type;

   type Full_Camera_State_Type is new Camera_State_Type with record
      Camera            : Standard.Camera.Commands.Camera_Class_Access := Null;
      Camera_ID         : Camera_ID_Type;
      Camera_Name       : Ada_Lib.Strings.Unlimited.String_Type;
      Camera_Pan        : Absolute_Type;
      Camera_Pan_Speed  : Property_Type;
      Camera_Tilt       : Absolute_Type;
      Camera_Tilt_Speed : Property_Type;
      Camera_Zoom       : Property_Type;
      Options           : Ada_Lib.Options.Interface_Options_Class_Access :=
                           Null;
   end record;

   type Full_Camera_State_Access
                        is access Full_Camera_State_Type;
-- type Full_Camera_State_Class_Access
--                      is access Full_Camera_State_Type'class;

   function Camera_State_Equal (
      Left, Right                : in     Full_Camera_State_Access
   ) return Boolean;

   overriding
   function Get_Camera (
      Camera_State      : in     Full_Camera_State_Type
   )return Camera.Commands.Camera_Class_Access;

   overriding
   function Get_Camera_ID (
      Camera_State      : in     Full_Camera_State_Type
   ) return Camera_ID_Type;

   overriding
   function Get_Camera_Name (
      Camera_State      : in     Full_Camera_State_Type
   ) return String;

   overriding
   function Get_Camera_State_Pan_Speed (
      Camera_State      : in     Full_Camera_State_Type
   ) return Data_Type;

   overriding
   function Get_Camera_State_Tilt_Speed (
      Camera_State      : in     Full_Camera_State_Type
   ) return Data_Type;

   overriding
   procedure Set_Mouse_Action (
      Camera_State      : in     Full_Camera_State_Type;
      Action            : in     Camera.Mouse_Click_Action_Type);

   ---------------------------------------------------------------
   function Allocate_Camera_State
   return Camera_State_Class_Access is
   ---------------------------------------------------------------

   begin
      return Camera_State_Class_Access'(new Full_Camera_State_Type);
   end Allocate_Camera_State;

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
--         GNOGA_Ada_Lib.Connection_Data_Class_Access (Base_Data));
--      return Base_Data;
--   end Allocate_Connection_Data;

   ----------------------------------------------------------------
   function Camera_State_Equal (
      Left, Right                : in     Full_Camera_State_Access
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end Camera_State_Equal;

   ----------------------------------------------------------------
   overriding
   function Get_Camera (
      Camera_State      : in     Full_Camera_State_Type
   )return Camera.Commands.Camera_Class_Access is
   ----------------------------------------------------------------

   begin
      return Camera_State.Camera;
   end Get_Camera;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_ID (
      Camera_State      : in     Full_Camera_State_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------

   begin
      return Camera_State.Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_Name (
      Camera_State      : in     Full_Camera_State_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Camera_State.Camera_Name.Coerce;
   end Get_Camera_Name;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_State_Pan_Speed (
      Camera_State      : in     Full_Camera_State_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Full_Camera_State_Type'class (Camera_State).Camera_Pan_Speed;
   end Get_Camera_State_Pan_Speed;

   ----------------------------------------------------------------
   overriding
   function Get_Camera_State_Tilt_Speed (
      Camera_State      : in     Full_Camera_State_Type
   ) return Data_Type is
   ----------------------------------------------------------------

   begin
      return Full_Camera_State_Type'class (Camera_State).Camera_Pan_Speed;
   end Get_Camera_State_Tilt_Speed;

-- ----------------------------------------------------------------
-- function Get_Camera_State (
--    Camera_State      : in     Camera_State_Type
-- ) return Camera_State_Class_Access is
-- ----------------------------------------------------------------
--
-- begin
--    return Camera_State_Class_Access (Full_Camera_State'access);
-- end Get_Camera_State;

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
--      Wait_For_Completion        : in     Boolean;
--      Handler_Path               : in     String := "default";
--      Verbose                    : in     Boolean := False) is
--   ---------------------------------------------------------------
--
--   begin
--      Log_In (Debug, "GNOGA_Initialized " & GNOGA_Initialized'img &
--         " Wait_For_Completion " & Wait_For_Completion'img &
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
--      Log_Here (Debug, "Wait_For_Completion " & Wait_For_Completion'img);
--
--      if Wait_For_Completion then
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
      Camera         : in out Camera_State_Type;
      Description    : in     Ada_Lib.Strings.String_Constant_Access) is
   ---------------------------------------------------------------

      State_Pointer     : constant Configuration.Camera.State.
                           State_Constant_Access := Standard.Camera.
                              States.Get_Read_Only_Configuration_State;
      State             : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
      Port_Number       : constant Standard.Camera.Port_Type :=
                           State.Get_Host_Port;
      Camera_Address    : constant Ada_Lib.Socket_IO.Address_Type :=
                           State.Get_Host_Address;
      Full_Camera_State : Full_Camera_State_Type renames
                           Full_Camera_State_Type (
                              Camera_State_Type'class (Camera));

   begin
      Log_In (Debug,
         Quote (" Camera_URL", Camera_Address.Image) &
         " port" & Port_Number'img);

      Full_Camera_State.Camera :=
         Standard.Camera.Commands.Camera_Class_Access'(
            new Standard.Camera.Commands.PTZ_Optics.PTZ_Optics_Type (
               Description));

      Full_Camera_State.Camera.Open (Camera_Address, Port_Number);
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
      Camera_State      : in     Full_Camera_State_Type;
      Action            : in     Mouse_Click_Action_Type) is
   ----------------------------------------------------------------

   begin
not_implemented;
   end Set_Mouse_Action;

begin
--Debug := True;
   Include_Task := True;
   Log_Here (Elaborate or Trace_Options);
end Camera.Base;
