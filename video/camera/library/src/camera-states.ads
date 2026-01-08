with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings.Unlimited;
limited with Camera.Base;
--limited with Camera.Lib.Base;
limited with Camera.Main;
limited with Camera.State;
limited with Configuration.Camera.State;
limited with Configuration.Camera.Setup;

package Camera.States is

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

   procedure Allocate_Connection_Data (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID);

-- function Get_Read_Only_Global_State (
--    Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
-- ) return access Camera.State.State_Type'class
-- with Pre    => Has_Camera_Configuration_State (Camera_ID);

   function Get_Writeable_Global_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Camera.State.State_Access
   with Pre    => Has_Camera_Configuration_State;

   function Has_Camera_Configuration_State (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean;

   function Has_Camera_State (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean;

   function Has_Camera_ID
   return Boolean;

   function Has_Camera_ID (
      Camera_ID            : in        Camera_ID_Type'class
   ) return Boolean;

   function Has_Window_Connection (
      Camera_ID            : in        Camera_ID_Type'class := Null_Camera_ID
   ) return Boolean;

   function Get_Camera_Names
   return Camera_Names_Type;

   function Get_Read_Only_Configuration_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Configuration.Camera.State.State_Constant_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Read_Only_Camera_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Camera.Base.Camera_Ready_Only_State_Class_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Writeable_Camera_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Camera.Base.Camera_State_Access
   with Pre    => Has_Camera_Configuration_State;

   function Get_Writeable_Configuration_State (
      Camera_ID   : Camera_ID_Type'class := Null_Camera_ID
   ) return Configuration.Camera.State.State_Access;

   function State_Equal (
      Left, Right       : State_Access
   ) return Boolean;

   package State_Package  is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type       => Camera_ID_Type'class,
      Element_Type   => State.State_Access,
      Hash           => Camera_Hash,
      Equivalent_Keys=> Camera_ID_Equal,
      "="            => State_Equal);

   Debug                   : Boolean := False;
   States                  : State_Package.Map;


private

   function Allocate_Camera_State (
      Camera_ID            : in        Camera_ID_Type'class := Camera.Null_Camera_ID
   ) return Camera.Base.Camera_State_Class_Access
   with Pre => Has_Camera_ID (Camera_ID);

   function Allocate_Configuration_State (
      Camera_ID            : in        Camera_ID_Type'class
   ) return Camera_Configuration_State_Access;

   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type'class
   ) return Camera_Main_Window_Connection_Class_Access;

end Camera.States;
