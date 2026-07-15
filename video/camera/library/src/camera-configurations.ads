--with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
limited with Camera.Base;
--limited with Camera.Lib.Base;
limited with Camera.Main;
with Camera.Configuration;
limited with Configuration.Camera.State;
limited with Configuration.Camera.Setup;

package Camera.Configurations is

-- use type Configuration.Configuration_Access;
-- use type Configuration.Configuration_Class_Access;

   type Camera_Base_Configuration_Class_Access
                     is access all Base.Configuration_Type'class;
-- type Camera_Base_Configuration_Constant_Class_Access
--                   is access constant Base.Configuration_Type'class;
   type Camera_Configuration_Setup_Access
                     is access all Standard.Configuration.Camera.Setup.Setup_Type;
   type Camera_Configuration_Setup_Class_Access
                     is access all Standard.Configuration.Camera.Setup.Setup_Type'class;
   type Camera_Configuration_Setup_Constant_Access
                     is access all Standard.Configuration.Camera.Setup.Setup_Type;
   type Camera_Configuration_Setup_Constant_Class_Access
                     is access all Standard.Configuration.Camera.Setup.Setup_Type'class;
   type Camera_Configuration_State_Access
                     is access all Standard.Configuration.Camera.State.State_Type;
   type Camera_Configuration_State_Class_Access
                     is access all Standard.Configuration.Camera.State.State_Type'class;
   type Camera_Configuration_State_Constant_Class_Access
                     is access constant Standard.Configuration.Camera.State.State_Type'class;
   type Camera_Main_Window_Connection_Class_Access
                     is access all Main.Window_Connection_Type'class;
   type Camera_Names_Type
                     is array (Positive range <>) of
                        Ada_Lib.Strings.Unlimited.String_Type;

   function Get_Camera_Configuration_Setup (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_Setup_Class_Access
   with Pre    => Has_Camera_Configuration_Setup (Camera_ID);

   function Get_Camera_Configuration_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_State_Class_Access
   with Pre    => Has_Camera_Configuration_State (Camera_ID);

   function Get_Camera_Names
   return Camera_Names_Type;

   function Get_Configuration (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Base.Configuration_Class_Access
   with Pre    => Has_Configuration (Camera_ID);

   function Get_Read_Only_Camera_Configuration_Setup (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_Setup_Constant_Access
   with Pre    => Has_Camera_Configuration_State (Camera_ID);

   function Get_Read_Only_Camera_Configuration_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_State_Constant_Class_Access
   with Pre    => Has_Camera_Configuration_State (Camera_ID);

   function Get_Read_Only_Configuration (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Configuration.Configuration_Constant_Access
   with Pre    => Has_Configuration (Camera_ID);

   function Has_Camera_Configuration_Setup (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Has_Camera_Configuration_State (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Has_Camera_ID (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Has_Configuration (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   procedure Set_State (
      Camera_ID      : in     Camera_ID_Type;
      Configuration  : access Camera.Base.Configuration_Type'class
   ) with Pre  => Camera_ID.Is_Set and then
                  Configuration /= Null;

   procedure Clear_Configuration;

private

-- function Get_Camera_Configuration (
--    Camera_ID            : in        Camera_ID_Type := Camera.Null_Camera_ID
-- ) return Camera.Base.Configuration_Class_Access
-- with Pre => Has_Camera_ID (Camera_ID);

-- function Get_Configuration_State (
--    Camera_ID            : in        Camera_ID_Type
-- ) return Camera_Configuration_State_Access;

   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type
   ) return Camera_Main_Window_Connection_Class_Access;

end Camera.Configurations;
