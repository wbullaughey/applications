--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Configuration;
with Ada_Lib.Options.Program;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Options;
with Camera.Lib.Unit_Test;
with Configuration.Camera.State;
with Configuration.Camera.Setup;

package body Camera.Configuration is

-- use type Standard.Configuration.Camera.State.State_Access;
-- use type Camera.Base.Camera_State_Class_Access;
--
-- procedure Free is new Ada.Unchecked_Deallocation (
--    Name     => Standard.Configuration.Camera.Setup.Setup_Access,
--    Object   => Standard.Configuration.Camera.Setup.Setup_Type);
--
-- procedure Free is new Ada.Unchecked_Deallocation (
--    Name     => Standard.Configuration.Camera.State.State_Access,
--    Object   => Standard.Configuration.Camera.State.State_Type);
--
-- Current_Camera_ID : Camera_ID_Type := Null_Camera_ID;
   Debug             : Boolean renames Lib.Options.Camera_Options.State_Debug;

   ----------------------------------------------------------------
   procedure Load (
      Configuration        : in out Configuration_Type;
      Setup_Name           : in     String;
      State_Name           : in     String) is
   ----------------------------------------------------------------

Ada_Lib_Options : constant Ada_Lib.Options.Program.Program_Options_Constant_Class_Access :=
Ada_Lib.Options.Program.Get_Read_Only_Program_Options;

begin
tag_history (true, Ada_Lib_Options.all'tag);
declare

      Options  : Camera.Lib.Unit_Test.
                     Unit_Test_Options_Constant_Class_Access :=
                  Camera.Lib.Unit_Test.
                        Unit_Test_Options_Constant_Class_Access (
                     Ada_Lib.Options.Program.Get_Read_Only_Program_Options);

   begin
      Standard.Camera.Configuration.State.Load (
         Options.Configuration.Get_Location, State_Name);
      Standard.Camera.Configuration.Setup.Load (Configuration.Configuration_State.all, State_Name);
end;
   end Load;

--   ----------------------------------------------------------------
--   function Resolve_ID (
--      Camera_ID   : in     Camera_ID_Type
--   ) return Camera_ID_Type is
--   ----------------------------------------------------------------
--
--   begin
--      if Camera_ID.Is_Set then
--         return Camera_ID;
--      elsif Current_Camera_ID.Is_Set then
--            return Current_Camera_ID;
--      else
--         raise Failed with "no current camera id";
--      end if;
--   end Resolve_ID;
--
--   ----------------------------------------------------------------
--   procedure Set_Camera_State (
--      State          : in out Configuration_Type;
--      Camera_State   : in     Base.Camera_State_Class_Access) is
--   ----------------------------------------------------------------
--
--   begin
--      State.Camera_State := Camera_State;
--   end Set_Camera_State;
--
--   ----------------------------------------------------------------
--   procedure Set_Current_Camera_ID (
--      Camera_ID   : in     Camera_ID_Type) is
--   ----------------------------------------------------------------
--
--   begin
--      Current_Camera_ID := Camera_ID;
--   end Set_Current_Camera_ID;

begin
   --Debug := False;
   Log_Here (Debug);
end Camera.Configuration;
