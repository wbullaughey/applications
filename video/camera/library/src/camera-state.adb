--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Configuration;
with Ada_Lib.Options.Program;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Base;
with Camera.Lib.Options;
--with Camera.Main;
with Configuration.Camera.State;
with Configuration.Camera.Setup;

package body Camera.State is

   Current_Camera_ID : Camera_ID_Type := Null_Camera_ID;
   Debug             : Boolean renames Lib.Options.Camera_Options.State_Debug;


   ----------------------------------------------------------------
   procedure Allocate (
      State    : in out State_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      State.Configuration_Setup := new Configuration.Camera.Setup.Setup_Type;
      State.Configuration_State := new Configuration.Camera.State.State_Type;
      Log_Out (Debug);
   end Allocate;

   ----------------------------------------------------------------
   function Get_Configuration_Setup (
      State       : in     State_Type
   ) return access Configuration.Camera.Setup.Setup_Type is
   ----------------------------------------------------------------

   begin
      return State.Configuration_Setup;
   end Get_Configuration_Setup;

   ----------------------------------------------------------------
   function Get_Configuration_State (
      State       : in     State_Type
   ) return access Configuration.Camera.State.State_Type is
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
log_here;
      return result;
end;
   end Get_Current_Camera_ID;

-- ----------------------------------------------------------------
-- function Get_Read_Only_Global_State (
--    Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
-- ) return State_Constant_Access is
-- ----------------------------------------------------------------
--
-- begin
--    return State_Constant_Access (Allocate_State);
-- end Get_Read_Only_Global_State;
--
   ----------------------------------------------------------------
   function Has_Configuration_Setup (
      State       : in     State_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Configuration_Setup /= Null, Debug);
   end Has_Configuration_Setup;

   ----------------------------------------------------------------
   function Has_Configuration_State (
      State       : in     State_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin

      return Log_Here (State.Configuration_State /= Null, Debug);
   end Has_Configuration_State;

   ----------------------------------------------------------------
   function Has_Current_Camera_ID
   return Boolean is
   ----------------------------------------------------------------

   begin
      return Current_Camera_ID.Set;
   end Has_Current_Camera_ID;

-- ----------------------------------------------------------------
-- procedure Load (
--    Location    : in     Video.Lib.Location_Type) is
-- ----------------------------------------------------------------
--
--    Cameras           : Ada_Lib.Configuration.Configuration_Type;
--    Current_Directory : constant String :=
--                         Standard.Camera.Lib.Options.Current_Directory;
--    File_Name         : constant String := "cameras.cfg";
--    Path              : constant String :=
--                         (if Current_Directory'length > 0 then
--                            Current_Directory & "/"
--                         else
--                            "") & File_Name;
--    State             : constant State_Access := new State_Type;
--
-- begin
--    Cameras.Load (Path, Create => False);
--    declare
--       Number_Cameras : constant Natural :=
--                         Cameras.Get_Integer ("number_cameras");
--    begin
--       for Camera in 1 .. Number_Cameras loop
--          declare
--             Camera_Number     : constant String :=
--                                  Ada_Lib.Strings.Trim (Camera'img);
--             State_Name        : constant String := Cameras.Get_String (
--                                  "state_" & Camera_Number);
--             Setup_Name        : constant String := Cameras.Get_String (
--                                  "setup_" & Camera_Number);
--          begin
--             Load (State.all, Setup_Name, State_Name);
--          end;
--       end loop;
--    end;
-- end Load;

   ----------------------------------------------------------------
   procedure Load (
      State                : in out State_Type;
      Setup_Name           : in     String;
      State_Name           : in     String) is
   ----------------------------------------------------------------

begin
log_here ("state");
tag_history (true, state'tag);
declare

      Options  : Camera.Lib.Options.Program_Options_Constant_Class_Access :=
                  Camera.Lib.Options.Program_Options_Constant_Class_Access (
                     Ada_Lib.Options.Program.Get_Read_Only_Program_Options);

   begin
      State.Configuration_State.Load (
         Options.Camera_Library.Camera_Options.Location, State_Name);
      State.Configuration_Setup.Load (State.Configuration_State.all, State_Name);
end;
   end Load;

   ----------------------------------------------------------------
   function Resolve_ID (
      Camera_ID   : in     Camera_ID_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------

   begin
      if Camera_ID.Is_Set then
         return Camera_ID;
      elsif Current_Camera_ID.Is_Set then
            return Current_Camera_ID;
      else
         raise Failed with "no current camera id";
      end if;
   end Resolve_ID;

   ----------------------------------------------------------------
   procedure Set_Current_Camera_ID (
      Camera_ID   : in     Camera_ID_Type) is
   ----------------------------------------------------------------

   begin
      Current_Camera_ID := Camera_ID;
   end Set_Current_Camera_ID;

begin
   --Debug := False;
   Log_Here (Debug);
end Camera.State;
