--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Base;
with Camera.Lib.Options;
--with Camera.Main;
--with Configuration.Camera.State;

package body Camera.State is

   procedure Load (
      State                : in out State_Type;
      Setup_Name           : in     String;
      State_Name           : in     String);

   ----------------------------------------------------------------
   function Allocate_State (
      Camera_ID            : in        Camera_ID_Type := Camera.Null_Camera_ID
   ) return State_Access is
   ----------------------------------------------------------------

      Lookup_Camera_ID     : constant Camera_ID_Type := (if Camera_ID.Set then
                                 Camera_ID
                              else
                                 Current_Camera_ID);
   begin
      Log_In (Debug, "ID:" &Lookup_Camera_ID'img);
      if States.Contains (Lookup_Camera_ID) then
         Log_Out (Debug, "current state");
         return State_Access (States.Element (Lookup_Camera_ID));
      else
         declare
            State    : constant State_Access := new State.State_Type;

         begin
            States.Insert (Lookup_Camera_ID, State);
            Log_Out (Debug, "new state");
            return State;
         end;
      end if;

   end Allocate_State;

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
   function Get_Writeable_Global_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return State_Access is
   ----------------------------------------------------------------

   begin
      return Allocate_State (Camera_ID);
   end Get_Writeable_Global_State;

   ----------------------------------------------------------------
   procedure Load (
      Location    : in     Video.Lib.Location_Type) is
   ----------------------------------------------------------------

      Cameras           : Ada_Lib.Configuration.Configuration_Type;
      Current_Directory : constant String :=
                           Standard.Camera.Lib.Options.Current_Directory;
      Path              : constant String :=
                           (if Current_Directory'length > 0 then
                              Current_Directory & "/"
                           else
                              "") & File_Name;
      State             : constant State_Access := new State_Type;

   begin
      Cameras.Load (Path, Create => False);
      declare
         Number_Cameras : constant Natural :=
                           Config.Get_Integer ("number_cameras");
      begin
         for Camera in 1 .. Number_Cameras loop
            declare
               Camera_Number     : constant String :=
                                    Ada_Lib.Strings.Trim (Camera'img);
               State_Name        : constant String := Config.Get_String (
                                    "state_" & Camera_Number);
               Setup_Name        : constant String := Config.Get_String (
                                    "setup_" & Camera_Number);
            begin
               Load (State.all, Setup_Name, State_Name);
            end;
         end loop;
      end;
   end Load;

   ----------------------------------------------------------------
   procedure Load (
      State                : in out State_Type;
      Setup_Name           : in     String;
      State_Name           : in     String) is
   ----------------------------------------------------------------

      State                : constant Stage_Access := new State_Type;

   begin
      State.Configuration_Setup.Load (State_Name);
      State.Configuration_State.Load (State_Name);
   end Load;

begin
   --Debug := False;
   Log_Here (Debug);
end Camera.State;
