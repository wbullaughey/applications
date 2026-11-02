--with Ada_Lib.Trace;
with Camera;
with Configuration.Configuration;
with Video.Lib;

package Configuration.Setup is

   Failed            : exception;

-- type Setup_Type   is new Configuration_Package.Configuration_Type
--                      with private;
-- type Setup_Access is access all Setup_Type;
--
--   function Setup_Label (
--      Setup_ID           : in     Setup_ID_Type
--   ) return String
--   with Pre => Setup.Has_Setup (Setup_ID);
--
---- function Configuration_Preset (
----    Configuration_ID           : in     Configuration_ID_Type
---- ) return Video.Lib.Preset_ID_Type
---- with Pre => Setup.Has_Configuration (Configuration_ID);
----
   procedure Dump (
      From           : in     String := Ada_Lib.Trace.Here);
--
--   procedure Free (
--      Setup                      : in     Setup_Type);
--
-- function Get_Configuration (
--    Configuration_ID           : in     Configuration_ID_Type
-- ) return Configuration_Type'class
-- with Pre => Setup.Has_Configuration (Configuration_ID);

   function Get_Number_Presets
   return Video.Lib.Preset_ID_Type;

-- function Get_Preset (
--    Preset_Id                  : in     Video.Lib.Preset_ID_Type
-- ) return Preset_Type'class
-- with Pre => Is_Loaded;

-- function Get_Preset_ID (
--    Row                        : in     Row_Type;
--    Column                     : in     Column_Type
-- ) return Video.Lib.Preset_ID_Type;
--
-- function Get_Preset_ID (
--    Configuration_Id           : in     Configuration_ID_Type
-- ) return Video.Lib.Preset_ID_Type
-- with Pre => Setup.Has_Configuration(Configuration_Id);
--
-- function Get_Preset_ID (
--    Configuration        : in     Setup_Type
-- ) return Video.Lib.Preset_ID_Type
-- with Pre => Configuration.Is_Loaded;

-- type Setups_Type              is array (Configuration_ID_Type range <>)
--                                  of Setup_Type ;
--
-- type Configurations_Access    is access Setups_Type;

-- procedure Load (
--    Configurations       : in out Configuration.Setup.Setups_Type;
--    Path                 : in     String;
--    Location             : in     Video.Lib.Location_Type
-- ) with Pre    => Path'length > 0;
--
   procedure Unload;

-- type Preset_Type
--                is new Configuration_Type with private;
--
-- procedure Dump (
--    Preset               : in     Preset_Type;
--    What                 : in     String := "";
--    From                 : in     String := Ada_Lib.Trace.Here);
--
-- function Get_Column (
--    Preset               : in     Preset_Type
-- ) return Column_Type
-- with Pre => Preset.Is_Loaded;
--
-- function Get_Preset_ID (
--    Preset               : in     Preset_Type
-- ) return Video.Lib.Preset_ID_Type
-- with Pre => Preset.Is_Loaded;
--
-- function Get_Row (
--    Preset               : in     Preset_Type
-- ) return Row_Type
-- with Pre => Preset.Is_Loaded;
--
-- function Make_Image_Name (
--    Preset               : in     Preset_Type
-- ) return String;
--
-- function Preset_Image (
--    Preset               : in     Preset_Type
-- ) return String;
--
-- type Presets_Type       is array (Video.Lib.Preset_Range_Type range <>)
--                            of Preset_Type;
--
-- type Presets_Access     is access Presets_Type;
--
-- function Has_Configuration (
--    Configuration_ID           : in     Configuration_ID_Type
-- ) return Boolean
-- with Pre => Is_Loaded;

-- function Has_Preset (
--    Preset_Id                  : in     Video.Lib.Preset_ID_Type
-- ) return Boolean
-- with Pre => Is_Loaded;

   function Is_Loaded
   return Boolean;

   procedure Load  (
      Name                       : in     String
   ) with Pre  => not Is_Loaded,
          Post => Is_Loaded;

-- function Make_Image_Name (
--    Row                        : in     Row_Type;
--    Column                     : in     Column_Type
-- ) return String;

-- function Get_Preset_Column (
--    Preset                     : in     Video.Lib.Preset_ID_Type
-- ) return Column_Type
-- with Pre => Setup.Has_Preset (Preset);
--
-- function Preset_Row (
--    Preset                     : in     Video.Lib.Preset_ID_Type
-- ) return Row_Type
-- with Pre => Is_Loaded;

   procedure Set_Path (
      Path                       : in     String);

-- procedure Unload (
--    State                      : in     Configuration.State.State_Type'class;
--    Save_Changes               : in     Boolean
-- ) with Pre => Is_Loaded;
--
   procedure Update_Configuration (
      Configuration_ID           : in     Configuration.Configuration_ID_Type;
      Label                      : in     String);

   procedure Update_Configuration (
      Configuration_ID           : in     Configuration.Configuration_ID_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type);

-- procedure Update_Preset (
--    Preset_ID                  : in     Video.Lib.Preset_ID_Type;
--    Row                        : in     Row_Type;
--    Column                     : in     Column_Type);

   procedure Update
   with Pre => Is_Loaded;


   Default_Setup                 : constant String := "setup.cfg";

-- Global_Camera_Setup           : Setup_Access := Null;

private

-- type Preset_Type  is new Configuration_Type with record
--    Column         : Column_Type := Column_Not_Set;
--    Preset_ID      : Video.Lib.Preset_ID_Type :=
--                      Video.Lib.Null_Preset_ID;
--    Row            : Row_Type := Row_Not_Set;
-- end record;

-- type Setup_Type   is new Configuration.Setup_Type with record
--    Configurations : Configurations_Access := Null;
--                      -- pointer to array of configurations
--    Modified       : Boolean := False;
--    Path           : Ada_Lib.Strings.Unlimited.String_Type;
--    Presets        : Presets_Access := Null;
--                      -- pointer to array of presets
-- end record;

-- Null_Setup            : constant Setup_Type := (
--                                  Configuration_Package.Configuration_Type with
--    Setup_ID          => Configuration_ID_Type'last,
--    Label             => Ada_Lib.Strings.Unlimited.Null_String);
--    Preset_ID         => Video.Lib.Null_Preset_ID);  -- ID field is 255 (last)

-- Null_Preset    : constant Preset_Type := (
--                   Configuration_Type with
--    Column      => Column_Not_Set,
--    Preset_ID   => Video.Lib.Null_Preset_ID,  -- ID field is 255 (last)
--    Row         => Row_Not_Set);

-- Null_Setup            : constant Setup_Type := (
--                                  Configuration.Setup_Type with
--    Configuration_ID  => Configuration_Not_Set,
--    Label             => Ada_Lib.Strings.Unlimited.Null_String,
--    Preset_ID         => Video.Lib.Null_Preset_ID);

end Configuration.Setup;
