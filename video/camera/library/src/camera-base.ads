with Ada.Exceptions;
with Ada_lib.Configuration;
with Ada_Lib.Options;
with Ada_Lib.Strings.Unlimited;
with Camera.Commands;
limited with Configuration.Camera.Setup;
limited with Configuration.Camera.State;
with GNAT.Source_Info;
with Gnoga.Gui.Window;
with Video.Lib;

package Camera.Base is

   Failed                        : exception;

   type Configuration_Type    is new Ada_lib.Configuration.
                                    Configuration_Type with private;
   type Configuration_Access        is access Configuration_Type;
   type Configuration_Constant_Class_Access
                                    is access constant Configuration_Type'class;
   type Configuration_Class_Access  is access all Configuration_Type'class;

   procedure Deallocate (
      Configuration        : in out Configuration_Type);

   function Get_Camera (
      Configuration        : in     Configuration_Type
   ) return Camera.Commands.Camera_Class_Access;
--
   function Get_Camera_Address (
      Configuration        : in     Configuration_Type
   ) return Camera.Address_Type;
--
   function Get_Camera_Name (
      Configuration       : in     Configuration_Type
   ) return String;

   function Get_Camera_ID (
      Configuration        : in     Configuration_Type
   ) return Camera_ID_Type;
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
   ) return access Standard.Configuration.Camera.State.State_Type
   with Pre    => Configuration.Has_Configuration;

   function Get_Video_Port (
      Configuration     : in     Configuration_Type
   ) return Video.Lib.Port_Type;

   function Has_Configuration_Setup (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   function Has_Configuration (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   function Has_Video_Address (
      Configuration      : in     Configuration_Type
   ) return Boolean;

   procedure Load (
      Configuration        : in out Configuration_Type;
      Path                 : in     String;
      Camera_Index         : in     Positive);

   procedure Load_Setup (
      Configuration        : in out Configuration_Type;
      Path                 : in     String);

   procedure Load_State (
      Configuration        : in out Configuration_Type;
      Path                 : in     String);

   type Configurations_Type is tagged limited private;
   type Camera_Read_Only_State_Access is access constant Configurations_Type;
   type Configurations_Access is access all Configurations_Type;
   type Configurations_Class_Access is access all Configurations_Type'class;
   type Camera_Ready_Only_State_Class_Access is
      access constant Configurations_Type'class;

   function Get_Configuration (
      Configurations          : in     Configurations_Type;
      Index                   : in     Positive
   ) return Configuration_Access;

   function Get_Number_Configurations (
      Configurations         : in     Configurations_Type
   ) return Natural;

   function Get_Read_Only_Configuration (
      Configurations         : in     Configurations_Type
   ) return Camera_Ready_Only_State_Class_Access;

   procedure Load (
      Configurations       : in out Configurations_Type;
      Path                 : in     String
   ) with Pre    => Path'length > 0;

   procedure Load (
      Location             : in     Video.Lib.Location_Type;
      Configuration_Name   : in     String
   ) with Pre => Configuration_Name'length > 0;

   procedure Halt;

   procedure Report_Exception (
      Window               : in out Gnoga.Gui.Window.Window_Type'class;
      Fault                : in     Ada.Exceptions.Exception_Occurrence;
      Message              : in     String;
      Where                : in     String := GNAT.Source_Info.Source_Location);

private

   type Configuration_Setup_Access
                        is access Configuration.Camera.Setup.Setup_Type;
   type Configuration_State_Access
                        is access Configuration.Camera.State.State_Type;

   type Configuration_Type    is new Ada_lib.Configuration.
                                    Configuration_Type with record
      Camera_ID            : Camera_ID_Type;
      Camera_Name          : Ada_Lib.Strings.Unlimited.String_Type;
      Configuration_Setup  : Configuration_Setup_Access := Null;
      Configuration_State  : Configuration_State_Access := Null;
      Default_Camera_Pan   : Absolute_Type;
      Default_Camera_Pan_Speed
                           : Property_Type;
      Default_Camera_Tilt  : Absolute_Type;
      Default_Camera_Tilt_Speed
                           : Property_Type;
      Default_Camera_Zoom  : Property_Type;
--    Location             : Video.Lib.Location_Type := Video.Lib.No_Location;
-- location is in options
      Setup_Path           : Ada_Lib.Strings.Unlimited.String_Type;
      Simulate             : Boolean := False;
      State_Path           : Ada_Lib.Strings.Unlimited.String_Type;
      Options              : Ada_Lib.Options.Flag_Option_Class_Access :=
                              Null;
   end record;

   type Configuration_Array
      is array (Positive range <>) of Configuration_Access;

   type Configuration_Access_Array
                              is access Configuration_Array;
   type Configurations_Type     is tagged limited record
      Configurations          : Configuration_Access_Array := Null;
      Number_Configurations   : Natural := 0;
   end record;

end Camera.Base;
