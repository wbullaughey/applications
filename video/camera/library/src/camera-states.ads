with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
limited with Camera.Base;
--limited with Camera.Lib.Base;
limited with Camera.Main;
with Camera.State;
limited with Configuration.Camera.State;
--with Configuration.State;
--limited with Configuration.Camera.Setup;

package Camera.States is

   use type State.State_Access;

   type Camera_Base_State_Class_Access
                     is access all Base.Camera_State_Type'class;
   type Camera_Configuration_State_Access
                     is access all Configuration.Camera.State.State_Type;
   type Camera_Configuration_State_Class_Access
                     is access all Configuration.Camera.State.State_Type'class;
   type Camera_Main_Window_Connection_Class_Access
                     is access all Main.Window_Connection_Type'class;
   type Camera_Names_Type
                     is array (Positive range <>) of
                        Ada_Lib.Strings.Unlimited.String_Type;
-- procedure Set_Camera_ID (
--    State                : in out State_Type;
--    Address              : in     Address_Type);

-- procedure Allocate_Connection_Data (
--    Camera_ID   : Camera_ID_Type := Null_Camera_ID);

-- function Get_Read_Only_Global_State (
--    Camera_ID   : Camera_ID_Type := Null_Camera_ID
-- ) return access Camera.State.State_Type'class
-- with Pre    => Has_Camera_Configuration_State (Camera_ID);

-- function Allocate_State (
--    Camera_ID   : in        Camera_ID_Type := Camera.Null_Camera_ID
-- ) return State.State_Access;

   function Get_Writeable_Global_State (
      Camera_ID   : Camera_ID_Type
   ) return State.State_Access
   with Pre => Camera_ID.Is_Set;

   function Has_Camera_Configuration_State (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Has_Camera_State (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

-- function Has_Camera_ID
-- return Boolean;

   function Has_Camera_ID (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Has_Window_Connection (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean;

   function Get_Camera_Names
   return Camera_Names_Type;

   function Get_Read_Only_Configuration_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Configuration.Camera.State.State_Constant_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Read_Only_Camera_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera.Base.Camera_Ready_Only_State_Class_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Writeable_Camera_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera.Base.Camera_State_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Writeable_Configuration_State ( -- allocates it if null
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Configuration.Camera.State.State_Access;

   procedure Set_State (
      Camera_ID      : in        Camera_ID_Type;
      Camera_State   : in        State.State_Access
   ) with Pre  => Camera_ID.Is_Set and then
                  Camera_State /= Null;

   function State_Equal (
      Left, Right       : State.State_Access
   ) return Boolean;

   package State_Package  is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type       => Camera_ID_Type,
      Element_Type   => State.State_Access,
      Hash           => Camera_ID_Hash,
      Equivalent_Keys=> Camera_ID_Equal,
      "="            => State_Equal);

   States                  : State_Package.Map;


private

   function Allocate_Camera_State (
      Camera_ID            : in        Camera_ID_Type := Camera.Null_Camera_ID
   ) return Camera.Base.Camera_State_Class_Access
   with Pre => Has_Camera_ID (Camera_ID);

   function Allocate_Configuration_State (
      Camera_ID            : in        Camera_ID_Type
   ) return Camera_Configuration_State_Access;

   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type
   ) return Camera_Main_Window_Connection_Class_Access;

end Camera.States;
