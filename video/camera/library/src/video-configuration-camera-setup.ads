with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;
with Camera;
with Video.Configuration.Camera.State;
with Video.Lib;
with Video.Preset;

package Video.Configuration.Camera.Setup is

   Failed         : exception;

   type Configuration_Type is new Configuration_Package.Configuration_Type
                              with private;

   type Configuration_Constant_Access
                           is access constant Configuration_Type;
   procedure Dump (
      Configuration        : in     Configuration_Type;
      What                 : in     String := "";
      From                 : in     String := Ada_Lib.Trace.Here);

   function Get_Preset_ID (
      Configuration        : in     Configuration_Type
   ) return Standard.Camera.Preset_ID_Type
   with Pre => Configuration.Is_Loaded;

   function Image (
      Configuration        : in     Configuration_Type
   ) return String;

   type Configurations_Type      is array (Configuration_ID_Type range <>)
                                    of aliased Configuration_Type ;

   type Configurations_Access    is access Configurations_Type;

   type Setup_Type         is new Configuration.Setup_Type with private;
   type Setup_Access       is access all Setup_Type;
   type Setup_Constant_Access
                           is access constant Setup_Type;
   type Setup_Class_Access
                           is access all Setup_Type'class;

   function Configuration_Label (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return String
   with Pre => Setup.Has_Configuration (Configuration_ID);

   function Configuration_Preset (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Standard.Camera.Preset_ID_Type
   with Pre => Setup.Has_Configuration (Configuration_ID);

   procedure Dump (
      Setup                      : in     Setup_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here);

   procedure Free (
      Setup                      : in     Setup_Type);

   function Get_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Configuration_Type'class
   with Pre => Setup.Has_Configuration (Configuration_ID);

-- function Get_Preset (
--    Setup                      : in     Setup_Type;
--    Preset_Id                  : in     Standard.Camera.Preset_ID_Type
-- ) return Preset_Type'class
-- with Pre => Setup.Is_Loaded;

   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Configuration_Id           : in     Configuration_ID_Type
   ) return Standard.Camera.Preset_ID_Type
   with Pre => Setup.Has_Configuration(Configuration_Id);

   function Has_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Boolean
   with Pre => Setup.Is_Loaded;

   function Has_Preset (
      Setup                      : in     Setup_Type;
      Camera_ID                  : in     Lib.Camera_ID_Type;
      Preset_Id                  : in     Standard.Camera.Preset_ID_Type
   ) return Boolean
   with Pre => Setup.Is_Loaded;

   function Has_Presets (
      Setup                      : in     Setup_Type;
      Expect                     : in     Boolean
   ) return Boolean;

   procedure Load  (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Name                       : in     String
   ) with Pre  => State.Is_Loaded and then
                  not Setup.Has_Presets (False),
          Post => Setup.Has_Presets (True);

   function Make_Image_Name (
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return String;

   function Preset_Column (
      Setup                      : in     Setup_Type;
      Preset                     : in     Standard.Camera.Preset_ID_Type
   ) return Column_Type
   with Pre => Setup.Has_Preset (Preset);

   function Preset_Row (
      Setup                      : in     Setup_Type;
      Preset                     : in     Standard.Camera.Preset_ID_Type
   ) return Row_Type
   with Pre => Setup.Is_Loaded;

   procedure Set_Path (
      Setup                      : in out Setup_Type;
      Path                       : in     String);

   procedure Unload (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Save_Changes               : in     Boolean
   ) with Pre => Setup.Is_Loaded;

   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Label                      : in     String);

   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Preset_ID                  : in     Standard.Camera.Preset_ID_Type);

   procedure Update_Preset (
      Setup                      : in out Setup_Type;
      Preset_ID                  : in     Standard.Camera.Preset_ID_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type);

   procedure Update  (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class
   ) with Pre => State.Is_Loaded;

   Default_Setup                 : constant String := "setup.cfg";

   Global_Camera_Setup           : Setup_Access := Null;
   Null_Configuration            : constant Configuration_Type;

private

   type Configuration_Type       is new Configuration_Package.Configuration_Type with record
      Configuration_ID           : Configuration_ID_Type;
      Label                      : Ada_Lib.Strings.Unlimited.String_Type;
      Preset_ID                  : Standard.Camera.Preset_ID_Type;
   end record;

   type Setup_Type   is new Configuration.Setup_Type with record
      Configurations : Configurations_Access := Null;
                        -- pointer to array of configurations
      Modified       : Boolean := False;
      Path           : Ada_Lib.Strings.Unlimited.String_Type;
--    Presets        : Presets_Access := Null;  local varialble in Video.Lib
--                      -- pointer to array of presets
   end record;

   Null_Configuration            : constant Configuration_Type := (
                                    Configuration_Package.Configuration_Type with
      Configuration_ID => Configuration_ID_Type'last,
      Label             => Ada_Lib.Strings.Unlimited.Null_String,
      Preset_ID         => Preset.Preset_ID_Not_Set);  -- ID field is 255 (last)

-- Null_Setup            : constant Setup_Type := (
--                                  Configuration.Setup_Type with
--    Configuration_ID  => Configuration_Not_Set,
--    Label             => Ada_Lib.Strings.Unlimited.Null_String,
--    Preset_ID         => Video.Lib.Preset_ID_Not_Set);

end Video.Configuration.Camera.Setup;
