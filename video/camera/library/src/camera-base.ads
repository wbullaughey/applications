with Ada.Exceptions;
with Ada_Lib.Strings.Unlimited;
with Camera.Commands;
limited with Configuration.Camera.Setup;
limited with Configuration.Camera.State;
with GNAT.Source_Info;
with Gnoga.Gui.Window;
with Video.Lib;

package Camera.Base is

   Failed                        : exception;

   type Configuration_Type          is tagged private;
   type Configuration_Access        is access Configuration_Type;
   type Configuration_Class_Access  is access all Configuration_Type'class;

-- function Get_Camera (
--    Configuration       : in     Configuration_Type
-- ) return Camera.Commands.Camera_Class_Access ;
--
-- function Get_Camera_Name (
--    Configuration       : in     Configuration_Type
-- ) return String ;
--
-- function Get_Camera_ID (
--    Configuration       : in     Configuration_Type
-- ) return Camera_ID_Type ;
--
-- function Get_Configurations_Pan_Speed (
--    Configuration       : in     Configuration_Type
-- ) return Data_Type ;
--
   function Get_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return access Standard.Configuration.Camera.Setup.Setup_Type
   with Pre    => Configuration.Has_Configuration_Setup;

   function Get_Configuration_State (
      Configuration        : in     Configuration_Type
   ) return access Standard.Configuration.Camera.State.State_Type'class
   with Pre    => Configuration.Has_Configuration_State;

-- function Get_Configurations_Tilt_Speed (
--    Configuration       : in     Configuration_Type
-- ) return Data_Type ;
--
   function Get_Current_Camera_ID
   return Camera_ID_Type;

   function Get_Location (
      Configuration       : in     Configuration_Type
   ) return Video.Lib.Location_Type;

-- function Get_Number_Columns (
--    Configuration       : in     Configuration_Type
-- ) return Standard.Configuration.Column_Type;
--
-- function Get_Number_Configurations (
--    Configuration       : in     Configuration_Type
-- ) return Standard.Configuration.Configuration_ID_Type;
--
-- function Get_Number_Rows (
--    Configuration       : in     Configuration_Type
-- ) return Standard.Configuration.Row_Type;
--
-- function Get_Setup_Path (
--    Configuration     : in     Configuration_Type
-- ) return String;
--
-- function Get_Simulate (
--    Configuration     : in     Configuration_Type
-- ) return Boolean;
--
-- function Get_State_Path (
--    Configuration     : in     Configuration_Type
-- ) return String;
--
   function Has_Camera_State (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   function Has_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   function Has_Configuration_State (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   function Has_Current_Camera_ID
   return Boolean;

   function Have_Video_Address (
      Configuration      : in     Configuration_Type
   ) return Boolean;

-- function Has_Image (
--    Row                  : in     Configuration.Row_Type;
--    Column               : in     Configuration.Column_Type
-- ) return Boolean;
--
-- function Image_Path (
--    Row                  : in     Configuration.Row_Type;
--    Column               : in     Configuration.Column_Type
-- ) return String;

   procedure Load (
      Configuration        : in out Configuration_Type;
      Path                 : in     String
   ) with Pre  => Path'length > 0;

   procedure Load_Setup (
      Configuration        : in out Configuration_Type);

   procedure Load_State (
      Configuration        : in out Configuration_Type);

   procedure Open_Camera (
      Configuration       : in out Configuration_Type;
      Description       : in     Ada_Lib.Strings.String_Constant_Access);

   procedure Set_Configuration_Setup (
      Configuration        : in out Configuration_Type;
      Configuration_Setup  : in     Standard.Configuration.Camera.Setup.
                                       Setup_Access);

   procedure Set_Configuration_State (
      Configuration        : in out Configuration_Type;
      Configuration_State  : in     Standard.Configuration.Camera.State.
                                       State_Access);

   procedure Set_Mouse_Action (
      Configuration       : in     Configuration_Type;
      Action            : in     Mouse_Click_Action_Type
   ) ;

   type Configurations_Type is tagged limited private;
   type Camera_Read_Only_State_Access is access constant Configurations_Type;
   type Configurations_Access is access all Configurations_Type;
   type Configurations_Class_Access is access all Configurations_Type'class;
   type Camera_Ready_Only_State_Class_Access is
      access constant Configurations_Type'class;

   function Get_Number_Configurations (
      Configurations         : in     Configurations_Type
   ) return Natural;

   function Get_Read_Only_Configuration (
      Configurations         : in     Configurations_Type
   ) return Camera_Ready_Only_State_Class_Access;

   function Get_Writeable_Configuration (
      Configurations         : in     Configurations_Type
   ) return Configurations_Class_Access;

   procedure Load (
      Configurations          : in out Configurations_Type;
      Path                    : in     String
   ) with Post    => Configurations.Get_Number_Configurations > 0;

-- function Allocate_Configurations
-- return Configurations_Class_Access;

   procedure Halt;

   procedure Report_Exception (
      Window               : in out Gnoga.Gui.Window.Window_Type'class;
      Fault                : in     Ada.Exceptions.Exception_Occurrence;
      Message              : in     String;
      Where                : in     String := GNAT.Source_Info.Source_Location);

   Debug                         : Boolean := False;

private

   type Configuration_Type is tagged record
      Configuration_Setup  : access Configuration.Camera.Setup.Setup_Type :=
                              Null;
      Configuration_State  : access Configuration.Camera.State.State_Type :=
                              Null;
      Location             : Video.Lib.Location_Type;
      Setup_Path           : Ada_Lib.Strings.Unlimited.String_Type;
      Simulate             : Boolean := False;
      State_Path           : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Configuration_Array   is array (Positive) of aliased Configuration_Type;

   type Configuration_Access_Array
                              is access Configuration_Array;
   type Configurations_Type     is tagged limited record
      Configurations          : Configuration_Access_Array := Null;
      Number_Configurations   : Natural := 0;
   end record;

end Camera.Base;
