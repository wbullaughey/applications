--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Base;
with Camera.Main;
with Configuration.Camera.State;

package body Camera.States is

   use type State.State_Access;

   ----------------------------------------------------------------
   procedure Allocate_Connection_Data (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID) is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

--    State       : constant Camera.Base.Camera_State_Access :=
--                   Allocate_Camera_State (Camera_ID);
--    pragma Unreferenced (State);

   begin
not_implemented;
   end Allocate_Connection_Data;

   ----------------------------------------------------------------
   function Allocate_Camera_State (
      Camera_ID            : in        Camera_ID_Type'class := Camera.Null_Camera_ID
   ) return Camera.Base.Camera_State_Class_Access is
   ----------------------------------------------------------------

--    State                : constant State_Access := Allocate_State (Camera_ID);

   begin
      Log_In (Debug, "ID:" &Camera_ID'img);
not_implemented;
--
--    if State.Camera_State = Null then
--       Log_Here (Debug, "debug new camera state");
--       State.Camera_State := Camera_Base_State_Class_Access (
--          Base.Allocate_Camera_State);
--    else
--       Log_Here ("debug current camera state");
--    end if;

      Log_Out (Debug);
--    return Camera.Base.Camera_State_Access (State.Camera_State);
return null;
   end Allocate_Camera_State;

   ----------------------------------------------------------------
   function Allocate_Configuration_State (
      Camera_ID            : in        Camera_ID_Type'class
   ) return Camera_Configuration_State_Access is
   ----------------------------------------------------------------

--    Camera_State   : constant State.State_Access :=
--                      Allocate_State (Camera_ID);

   begin
      Log_In (Debug, "ID:" &Camera_ID'img);
not_implemented;
--    if Camera_State.Configuration_State = Null then
--       Log_Here (Debug, "new camera state");
--       Camera_State.Configuration_State := new Configuration.Camera.State.State_Type;
--    else
--       Log_Here ("debug current camera state");
--    end if;
--
--    Log_Out (Debug, "state " & Image (Camera_State.Configuration_State.all'address));
--    return Camera_State.Configuration_State;
return null;
   end Allocate_Configuration_State;

   ----------------------------------------------------------------
   function Allocate_State (
      Camera_ID   : in        Camera_ID_Type'class := Camera.Null_Camera_ID
   ) return State.State_Access is
   ----------------------------------------------------------------

      Lookup_Camera_ID  : constant Camera_ID_Type :=
                           (if Camera_ID.Set then
                                 Camera_ID_Type (Camera_ID)
                              else
                                 State.Get_Current_Camera_ID);
   begin
      Log_In (Debug, "ID:" &Lookup_Camera_ID'img);
      if States.Contains (Lookup_Camera_ID) then
         Log_Out (Debug, "current state");
         return State.State_Access (States.Element (Lookup_Camera_ID));
      else
         declare
            Camera_State    : constant State.State_Access := new State.State_Type;

         begin
            States.Insert (Lookup_Camera_ID, Camera_State);
            Log_Out (Debug, "new state");
            return Camera_State;
         end;
      end if;

   end Allocate_State;

   ----------------------------------------------------------------
   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type'class
   ) return Camera_Main_Window_Connection_Class_Access is
   ----------------------------------------------------------------

--    Camera_State         : constant State.State_Access :=
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

   ----------------------------------------------------------------
   function Get_Read_Only_Camera_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Camera.Base.Camera_Ready_Only_State_Class_Access is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

   begin
not_implemented;
return null;
--    return Allocate_Camera_State (Camera_ID);
   end Get_Read_Only_Camera_State;

   ----------------------------------------------------------------
   function Get_Read_Only_Configuration_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   )return Configuration.Camera.State.State_Constant_Access is
   ----------------------------------------------------------------

   begin
      return Configuration.Camera.State.State_Constant_Access (
         Allocate_Configuration_State (Camera_ID));
   end Get_Read_Only_Configuration_State;

   ----------------------------------------------------------------
   function Get_Writeable_Camera_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Camera.Base.Camera_State_Access is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

   begin
not_implemented;
return null;
--    return Allocate_Camera_State (Camera_ID);
   end Get_Writeable_Camera_State;

   ----------------------------------------------------------------
   function Get_Writeable_Configuration_State (
      Camera_ID   : in     Camera_ID_Type'class := Null_Camera_ID
   ) return Configuration.Camera.State.State_Access is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

      Camera_State   : constant State.State_Access :=
                        Allocate_State (Camera_ID);

   begin
      Log_Here (Debug, "camera id" & Camera_ID.Image);

      if not Camera_State.Has_Configuration_State then
         Camera_State.Allocate;
      end if;

      return Configuration.Camera.State.State_Access (
         Camera_State.Get_Configuration_State);
   end Get_Writeable_Configuration_State;

   ----------------------------------------------------------------
   function Get_Writeable_Global_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return State.State_Access is
   ----------------------------------------------------------------

   begin
      return Allocate_State (Camera_ID);
   end Get_Writeable_Global_State;

   ----------------------------------------------------------------
   function Has_Camera_Configuration_State (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean is
   pragma Unreferenced (Camera_ID);
   ----------------------------------------------------------------

      Result   : constant Boolean := Has_Camera_Configuration_State (
                                       State.Get_Current_Camera_ID);
   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result);
   end Has_Camera_Configuration_State;

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
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean is
   ----------------------------------------------------------------

      Result   : constant Boolean := Camera_ID.Set or else
                                       State.Has_Current_Camera_ID;
   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result,
         "current: " & State.Get_Current_Camera_ID.Image &
         " parameter " & Camera_ID.Image);
   end Has_Camera_ID;

   ----------------------------------------------------------------
   function Has_Camera_State (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean is
   ----------------------------------------------------------------

      Camera_State   : constant State.State_Access :=
                        Allocate_State (Camera_ID);
      Result         : constant Boolean := Camera_State /= Null;

   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result,
         "Camera state for" & Camera_ID'img & " not allocated");
   end Has_Camera_State;

   ----------------------------------------------------------------
   function Has_Window_Connection (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
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
--    Camera_ID                  : in     Standard.Camera.Camera_ID_Type'class) is
-- ----------------------------------------------------------------
--
-- begin
--    Current_Camera_ID := Camera_ID;
-- end Set_Current_Camera_ID;

   ----------------------------------------------------------------
   function State_Equal (
      Left, Right       : State.State_Access
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end State_Equal;

begin
   --Debug := False;
   Log_Here (Debug);
end Camera.States;
