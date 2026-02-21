--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Base;
with Camera.Lib.Options;
with Camera.Main;
with Configuration.Camera.Setup;
with Configuration.Camera.State;

package body Camera.Configurations is

   use type Configuration.Configuration_Access;
   use type State_Package.Cursor;

   Debug    : Boolean renames Lib.Options.Camera_Options.States_Debug;

   ----------------------------------------------------------------
   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type
   ) return Camera_Main_Window_Connection_Class_Access is
   ----------------------------------------------------------------

--    Camera_State         : constant Configuration.Configuration_Access :=
--                            Allocate_State (Camera_ID);

   begin
      Log_In (Debug, "ID:" &Camera_ID'img);
not_implemented;
--    if Camera_State.Window_Connection = Null then
--       Log_Here ("debug new camera state");
--       Camera_State.Window_Connection := Camera_Main_Window_Connection_Class_Access (
--          Main.Allocate_Window_Connection);
--    else
--       Log_Here ("debug current camera state");
--    end if;
--
--    Log_Out (Debug);
--    return Camera_State.Window_Connection;
return null;
   end Allocate_Window_Connection;

   ----------------------------------------------------------------
   function Get_Camera_Names
   return Camera_Names_Type is
   ----------------------------------------------------------------

--    Index       : Natural := 0;
--    Result      : Camera_Names_Type (1 .. Positive (States.Length));

   begin
not_implemented;
return (1 .. 0 => <>);
--    for State of States loop
--       Index := Index + 1;
--       Result (Index).Construct (State.Camera_State.Get_Camera_Name);
--    end loop;
--
--    return Result;
   end Get_Camera_Names;

--   ----------------------------------------------------------------
--   function Get_Camera_Configuration (
--      Camera_ID            : in        Camera_ID_Type := Camera.Null_Camera_ID
--   ) return Camera_Base_Configuration_Class_Access is
--   ----------------------------------------------------------------
--
----    Global_State : constant Camera.Configuration.Configuration_Access :=
----                      Get_Writeable_Camera_Configuration (Camera_ID);
--
--   begin
--      Log_In (Debug, "ID:" &Camera_ID'img);
--
--      if Has_Camera_Configuration (Camera_ID) then
--         Log_Out (Debug, "debug current camera state");
--         return Global_State.Get_Camera_State;
--      else
--         declare
--            Camera_State   : constant Base.Configuration_Class_Access :=
--                              Base.Allocate_Camera_State;
--         begin
--            Log_Out (Debug, "debug new camera state");
--            Global_State.Set_Camera_State (Camera_State);
--            return Camera_State;
--         end;
--      end if;
--
--      Log_Out (Debug);
--   end Get_Camera_Configuration;

--   ----------------------------------------------------------------
--   function Get_Configuration_Setup (
--      Camera_ID            : in        Camera_ID_Type
--   ) return Standard.Configuration.Camera.Setup.Setup_Access is
--   ----------------------------------------------------------------
--
--      Configuration   : constant Configuration.Configuration_Access :=
--                        Get_Writeable_Camera_Configuration (Camera_ID);
--   begin
--      Log_In (Debug, "ID:" &Camera_ID'img);
--      if Configuration.Has_Configuration_Setup then
--         Log_Out (Debug, "current configuration Setup ");
----          Image (Camera_Setup.Configuration_Setup.all'address));
--         return Configuration.Get_Configuration_Setup;
--      else
--         declare
--            Configuration_Setup
--               : constant Standard.Configuration.Camera.Setup.Setup_Access :=
--                           new Standard.Configuration.Camera.Setup.Setup_Type;
--         begin
--            Log_Out (Debug, "new configuration Setup " &
--               Image (Configuration_Setup.all'address));
--            Configuration.Set_Configuration_Setup (Configuration_Setup);
--         end;
--      end if;
--
--   end Get_Configuration_Setup;
--
--   ----------------------------------------------------------------
--   function Get_Configuration_State (
--      Camera_ID            : in        Camera_ID_Type
--   ) return Camera_Configuration_State_Access is
--   ----------------------------------------------------------------
--
--      Configuration   : constant Configuration.Configuration_Access :=
--                        Get_Writeable_Camera_Configuration (Camera_ID);
--   begin
--      Log_In (Debug, "ID:" &Camera_ID'img);
--      if Configuration.Has_Configuration_State then
--         Log_Out (Debug, "current configuration state ");
----          Image (Camera_State.Configuration_State.all'address));
--         return Camera_Configuration_State_Access (
--            Configuration.Get_Configuration_State);
--      else
--         declare
--            Configuration_State
--               : constant Standard.Configuration.Camera.State.State_Access :=
--                           new Standard.Configuration.Camera.State.State_Type;
--         begin
--            Log_Out (Debug, "new configuration state " &
--               Image (Configuration_State.all'address));
--            Configuration.Set_Configuration_State (Configuration_State);
--         end;
--      end if;
--
--   end Get_Configuration_State;

   ----------------------------------------------------------------
   function Get_Read_Only_Configuration (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Configuration.Configuration_Constant_Class_Access is
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_Constant_Class_Access (
         Get_Writeable_Configuration (Camera_ID));
   end Get_Read_Only_Configuration;

-- ----------------------------------------------------------------
-- function Get_Read_Only_Configuration_State (
--    Camera_ID   : Camera_ID_Type := Null_Camera_ID
-- )return Standard.Configuration.Camera.State.State_Constant_Access is
-- ----------------------------------------------------------------
--
-- begin
--    return Standard.Configuration.Camera.State.State_Constant_Access (
--       Get_Configuration_State (Camera_ID));
-- end Get_Read_Only_Configuration_State;

--   ----------------------------------------------------------------
--   function Get_Writeable_Camera_State (
--      Camera_ID   : Camera_ID_Type := Null_Camera_ID
--   ) return Base.Configuration_Access is
--   pragma Unreferenced (Camera_ID);
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--return null;
----    return Allocate_Camera_State (Camera_ID);
--   end Get_Writeable_Camera_State;

-- ----------------------------------------------------------------
-- function Get_Writeable_Configuration_State (
--    Camera_ID   : in     Camera_ID_Type := Null_Camera_ID
-- ) return Standard.Configuration.Camera.State.State_Access is
-- ----------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "camera id " & Camera_ID.Image);
--
--    declare
--
--       Camera_State   : constant Configuration.Configuration_Access :=
--                         Configuration.Configuration_Access (
--                            State_Package.Element (States,
--                               State.Resolve_ID (Camera_ID)));
--    begin
--       if not Camera_State.Has_Configuration_State then
--          Log_Here (Debug);
--          Camera_State.Allocate;
--       end if;
--
--       return Standard.Configuration.Camera.State.State_Access (
--          Camera_State.Get_Configuration_State);
--    end;
-- end Get_Writeable_Configuration_State;

   ----------------------------------------------------------------
   function Get_Writeable_Configuration (
      Camera_ID   : Camera_ID_Type
   ) return Configuration.Configuration_Access is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "camera id " & Camera_ID.Image);
      declare
         Configuration   : Camera.Configuration.Configuration_Access :=
                           Camera.Configuration.Configuration_Access (
                              State_Package.Element (States, Camera_ID));
      begin
         if Configuration = Null then
            Log_Here (Debug, "create state");
            Configuration := new Camera.Configuration.Configuration_Type;

            State_Package.Insert (States, Camera_ID, Configuration);
         end if;

         Log_Out (Debug);
         return Configuration;
      end;
   end Get_Writeable_Configuration;

   ----------------------------------------------------------------
   function Has_Camera_Configuration (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean is
   ----------------------------------------------------------------

      Result   : constant Boolean := State_Package.Find (States, Camera_ID) /=
                  State_Package.No_Element;
   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result,
         "Camera state for" & Camera_ID'img & " not allocated");
   end Has_Camera_Configuration;

--   ----------------------------------------------------------------
--   function Has_Camera_Configuration_State (
--      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
--   ) return Boolean is
--   pragma Unreferenced (Camera_ID);
--   ----------------------------------------------------------------
--
--      Trace_Log   : constant Boolean := Debug or else Trace_Pre_Post_Conditions;
--
--begin
--log_here;
--      if State.Has_Current_Camera_ID then
--log_here;
--         declare
--            Camera_State   : constant Configuration.Configuration_Access :=
--                              Get_Writeable_Camera_Configuration (
--                                 State.Get_Current_Camera_ID);
--            Result   : constant Boolean := Camera_State.Has_Configuration_State;
--
--         begin
--            return Log_Here (Result, Trace_Log or else not Result);
--         end;
--      else
--log_here;
--         return Log_Here (False, Trace_Log);
--      end if;
--   end Has_Camera_Configuration_State;

-- ----------------------------------------------------------------
-- function Has_Camera_ID
-- return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Log_Here (Current_Camera_ID.Set,
--       Debug or else Trace_Pre_Post_Conditions or else
--       not Current_Camera_ID.Set, Current_Camera_ID.Image);
-- end Has_Camera_ID;

   ----------------------------------------------------------------
   function Has_Camera_ID (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean is
   ----------------------------------------------------------------

      Result   : constant Boolean := Camera_ID.Set or else
                                       Base.Has_Current_Camera_ID;
   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result,
         "current: " & Base.Get_Current_Camera_ID.Image &
         " parameter " & Camera_ID.Image);
   end Has_Camera_ID;

   ----------------------------------------------------------------
   function Has_Window_Connection (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

--    State          : constant State_Access := Allocate_State (Camera_ID);
--    Result   : constant Boolean := State.Window_Connection /= Null;

   begin
not_implemented;
return False;
--    return Log_Here (Result,
--       Debug or else Trace_Pre_Post_Conditions or else not Result,
--       "Window Connection for " & Camera_ID'img & " not allocated");
   end Has_Window_Connection;

--   ----------------------------------------------------------------
--   procedure Set_Camera_ID (
--      State                : in out State_Type;
--      Address              : in     Address_Type) is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--   end Set_Camera_ID;
--
-- ----------------------------------------------------------------
-- procedure Set_Current_Camera_ID (
--    Camera_ID                  : in     Standard.Camera.Camera_ID_Type) is
-- ----------------------------------------------------------------
--
-- begin
--    Current_Camera_ID := Camera_ID;
-- end Set_Current_Camera_ID;

-- ----------------------------------------------------------------
-- procedure Set_State (
--    Camera_ID      : in        Camera_ID_Type;
--    Camera_State   : in        Configuration.Configuration_Access) is
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "ID:" & Camera_ID'img);
--    State_Package.Insert (States, Camera_ID, Camera_State);
--    State.Set_Current_Camera_ID (Camera_ID);
--    Log_Out (Debug, "state set");
-- end Set_State;

   ----------------------------------------------------------------
   function State_Equal (
      Left, Right       : Configuration.Configuration_Access
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end State_Equal;

begin
   --Debug := False;
   Log_Here (Debug);
end Camera.Configurations;
