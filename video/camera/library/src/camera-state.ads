--with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Camera.Base;
--with Camera.Main;
--limited with Camera.States;
--limited with Camera.Lib.Base;
limited with Camera.Main;
limited with Configuration.Camera.State;
limited with Configuration.Camera.Setup;

package Camera.State is

   type State_Type                  is tagged private;
   type State_Access                is access all State_Type;
   type State_Constant_Access       is access all State_Type;
   type State_Class_Access          is access State_Type'class;
   type State_Constant_Class_Access is access constant State_Type'class;

   procedure Allocate (
      State    : in out State_Type
   ) with Pre  => not State.Has_Configuration_State and then
                  not State.Has_Configuration_Setup;

   function Get_Configuration_Setup (
      State       : in     State_Type
   ) return access Configuration.Camera.Setup.Setup_Type
   with Pre    => State.Has_Configuration_Setup;

   function Get_Configuration_State (
      State       : in     State_Type
   ) return access Configuration.Camera.State.State_Type
   with Pre    => State.Has_Configuration_State;

   function Get_Current_Camera_ID
   return Camera_ID_Type;

   function Has_Configuration_Setup (
      State       : in     State_Type
   ) return Boolean;

   function Has_Configuration_State (
      State       : in     State_Type
   ) return Boolean;

   function Has_Current_Camera_ID
   return Boolean;

-- procedure Load (
--    Location    : in     Video.Lib.Location_Type);

   procedure Load (
      State                : in out State_Type;
      Setup_Name           : in     String;
      State_Name           : in     String);

   function Resolve_ID (
      Camera_ID   : Camera_ID_Type
   ) return Camera_ID_Type;

   procedure Set_Current_Camera_ID (
      Camera_ID   : in     Camera_ID_Type
   ) with Pre  => Camera_ID.Is_Set;

private

   type State_Type         is tagged record
      Camera_State         : Base.Camera_State_Access := Null;
      Configuration_Setup  : access Configuration.Camera.Setup.Setup_Type;
      Configuration_State  : access Configuration.Camera.State.State_Type;
      Window_Connection    : access Main.Window_Connection_Type'class := Null;
   end record;

end Camera.State;
