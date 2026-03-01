with Ada_Lib.Configuration;
with Ada_Lib.Socket_IO;
with ADA_LIB.Strings;
with Video.Lib;

package Configuration.State is

   Failed                        : exception;

   use type Ada_Lib.Socket_IO.Address_Constant_Access;
   use type Video.Lib.Port_Type;
-- use type Video.Lib.Preset_ID_Type;

   subtype Location_Type   is Video.Lib.Location_Type;
   Local                   : Location_Type renames Video.Lib.Local;
   No_Location             : Location_Type renames Video.Lib.No_Location;
   Remote                  : Location_Type renames Video.Lib.Remote;

   type Address_Key_Type    is array (Location_Type) of
                               ADA_LIB.Strings.String_Access;

   type State_Type   is abstract new Root_State_Type with record
      Video_Address  : aliased Ada_Lib.Socket_IO.Address_Constant_Access :=
                        Null;
      Video_Port     : Video.Lib.Port_Type :=
                        Video.Lib.Port_Type'last;
   end record;

   type State_Access             is access State_Type;
   type State_Constant_Access    is access constant State_Type;

   procedure Dump (
      State                      : in     State_Type);

   function Get_Host_Address (
      State                      : in     State_Type
   ) return Ada_Lib.Socket_IO.Address_Type
   with Pre => State.Video_Address /= Null;

   function Get_Host_Port (
      State                      : in     State_Type
   ) return Video.Lib.Port_Type
   with Pre => State.Video_Port /= Video.Lib.Port_Type'last;

-- function Get_Number_Columns (
--    State                      : in     State_Type
-- ) return Column_Type is abstract;

-- function Get_Number_Configurations (
--    State                      : in     State_Type
-- ) return Configuration_ID_Type is abstract;
--
-- function Get_Number_Presets (
--    State                      : in     State_Type
-- ) return Natural is abstract;
--
-- function Get_Number_Rows (
--    State                      : in     State_Type
-- ) return Row_Type is abstract;

   function Have_Video_Address (
      State                      : in     State_Type
   ) return Boolean;

   function Have_Video_Port (
      State                      : in     State_Type
   ) return Boolean;

   procedure Load (
      State       : in out State_Type;
      Config      : in out Ada_Lib.Configuration.Configuration_Type;
      Location    : in     Location_Type;
      File_Name   : in     String);

   procedure Load (
      State                      : in out State_Type;
      Location                   : in     Location_Type;
      Name                       : in     String);

   procedure Unload (
      State                      : in out State_Type);

-- Global_Configuration_State            : State_Access := Null;

end Configuration.State;
