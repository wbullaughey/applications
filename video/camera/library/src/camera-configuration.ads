--with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Camera.Base;
--with Camera.Main;
--limited with Camera.Configurations;
--limited with Camera.Lib.Base;
limited with Camera.Main;
limited with Configuration.Camera.State;
limited with Configuration.Camera.Setup;

package Camera.Configuration is

   type Configuration_Type               is new Base.Configuration_Type with private;
   type Configuration_Access             is access all Configuration_Type;
   type Configuration_Constant_Access    is access all Configuration_Type;
   type Configuration_Class_Access       is access Configuration_Type'class;
   type Configuration_Constant_Class_Access
                                 is access constant Configuration_Type'class;

-- function Allocate;
-- return Configuration_Access;
-- ) with Pre  => not State.Has_Configuration_State and then
--                not State.Has_Configuration_Setup,
--        Post => State.Has_Configuration_State and then
--                State.Has_Configuration_Setup;

-- procedure Deallocate (
--    State    : in out Configuration_Type
-- ) with Pre  => State.Has_Configuration_State and then
--                State.Has_Configuration_Setup,
--        Post => not State.Has_Configuration_State and then
--                not State.Has_Configuration_Setup;

-- function Get_Camera_Configuration (
--    Configuration      : in     Configuration_Type
-- ) return Base.Camera_Configuration_Class_Access
-- with Pre => State.Has_Camera_State;

-- function Get_Current_Camera_ID
-- return Camera_ID_Type;
--
-- function Has_Current_Camera_ID
-- return Boolean;
--
-- procedure Load (
--    Location    : in     Video.Lib.Location_Type);

   procedure Load (
      Configuration        : in out Configuration_Type;
      Setup_Name           : in     String;
      State_Name           : in     String
   ) with   Pre => Configuration.Has_Configuration_State;

-- function Resolve_ID (
--    Camera_ID   : Camera_ID_Type
-- ) return Camera_ID_Type;

-- procedure Set_Camera_State (
--    State          : in out Configuration_Type;
--    Camera_State   : in     Base.Camera_Configuration_Class_Access
-- ) with Pre  => not State.Has_Camera_State;

-- procedure Set_Current_Camera_ID (
--    Camera_ID   : in     Camera_ID_Type
-- ) with Pre  => Camera_ID.Is_Set;

private

   type Configuration_Type         is new Base.Configuration_Type with record
      Window_Connection    : access Main.Window_Connection_Type'class := Null;
   end record;

end Camera.Configuration;
