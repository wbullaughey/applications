with Ada_Lib.Configuration;
with Ada_Lib.Socket_IO;
with Ada_Lib.Trace;
with Video.Lib;

package Configuration is

   subtype Address_Kind_Type  is Ada_Lib.Socket_IO.Address_Kind_Type;

   type Column_Type           is new Positive;
   type Configuration_ID_Type is new Positive;
   type Row_Type              is new Positive;
   type Speed_Type            is new Positive;

   IP                         : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.IP;
   No_Configuration           : constant Configuration_ID_Type :=
                                 Configuration_ID_Type'last;
   NOT_SET                    : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.NOT_SET;
   Row_Not_Set                : constant := Row_Type'last;
   URL                        : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.URL;

   package Configuration_Package is
      type Configuration_Type    is abstract tagged private;

      procedure Dump (
         Setup          : in     Configuration_Type;
         What           : in     String := "";
         From           : in     String := Ada_Lib.Trace.Here) is abstract;

      function Is_Loaded (
         Configuration           : in     Configuration_Type;
         From                    : in     String := Ada_Lib.Trace.Here
      ) return Boolean;

      procedure Set_Loaded (
         Configuration           : in out Configuration_Type;
         Value                   : in     Boolean;
         From                    : in     String := Ada_Lib.Trace.Here
      ) with Pre => Value /= Configuration.Is_Loaded,
             Post => Value = Configuration.Is_Loaded;

      procedure Unload (
         Configuration            : in out Configuration_Type
      ) with Pre => Configuration.Is_Loaded;

   private

      type Configuration_Type       is abstract tagged record
         Loaded                     : Boolean := False;
      end record;

   end Configuration_Package;

   type Setup_Type      is abstract new Configuration_Package.
                           Configuration_Type with null record;

-- procedure Load (
--    Setup          : in out Setup_Type;
--    State          : in     Configuration.Camera.State.State_Type'class;
--    Name           : in     String) is abstract;

   type State_Type   is abstract new Configuration_Package.
                        Configuration_Type with private;

   function Get_Video_Address (
      Camera                  : in     State_Type
   ) return Ada_Lib.Socket_IO.Address_Constant_Access is abstract;
-- with Pre => State.Has_Video_Address;

   function Get_Video_Address_URL (
      Camera                  : in     State_Type
   ) return String is abstract;
-- with Pre => State.Has_Video_Address and then
--             State.Is_URL_Video_Address;

   function Get_Video_Port (
      Camera                  : in     State_Type
   ) return Video.Lib.Port_Type is abstract;
-- with Pre => State.Has_Video_Port;

-- function Get_Number_Columns (
--    State                      : in     Root_Setup_Type
-- ) return Column_Type is abstract;

-- function Get_Number_Configurations (
--    State                      : in     Root_Setup_Type
-- ) return Configuration_ID_Type is abstract;
--
-- function Get_Number_Presets (
--    State                      : in     Root_Setup_Type
-- ) return Natural is abstract;
--
-- function Get_Number_Rows (
--    State                      : in     Root_Setup_Type
-- ) return Row_Type is abstract;

   function Has_Video_Address (
      Camera                      : in     State_Type
   ) return Boolean is abstract;

   function Has_Video_Port (
      Camera                      : in     State_Type
   ) return Boolean is abstract;

   function Is_URL_Video_Address (
      Camera                      : in     State_Type
   ) return Boolean is abstract;

   procedure Load (
      State       : in out State_Type;
      Config      : in out Ada_Lib.Configuration.Configuration_Type;
      Location    : in     Video.Lib.Location_Type;
      File_Name   : in     String) is abstract;

   function Have_Camera
   return Boolean;

   procedure No_Camera;

   Debug                         : Boolean := False;

private

   type State_Type   is abstract new Configuration_Package.
                        Configuration_Type with record
      Updated                    : Boolean := False;
   end record;

end Configuration;
