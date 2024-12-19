with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;
with Configuration.Camera.State;

package Configuration.Camera.Setup is

   Failed                        : exception;

   type Configuration_Type       is new Base_Type with record
      Configuration_ID           : Configuration_ID_Type;
      Label                      : Ada_Lib.Strings.Unlimited.String_Type;
      Preset_ID                  : Preset_ID_Type := Preset_Not_Set;
   end record;

   procedure Dump (
      Configuration              : in     Configuration_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

   type Preset_Type              is new Base_Type with record
      Column                     : Column_Type := Column_Not_Set;
      Row                        : Row_Type := Row_Not_Set;
      Preset_ID                  : Preset_ID_Type := Preset_Not_Set;
   end record;

   procedure Dump (
      Preset                     : in     Preset_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

   function Make_Image_Name (
      Preset                     : in     Preset_Type
   ) return String;

   function Preset_Image (
      Preset                     : in     Preset_Type
   ) return String;

   type Configurations_Type      is array (Configuration_ID_Type range <>)
                                    of Configuration_Type ;

   type Configurations_Access    is access Configurations_Type;

   type Presets_Type             is array (Preset_ID_Type range <>)
                                    of Preset_Type;

   type Presets_Access           is access Presets_Type;

   type Setup_Type               is tagged record
      Configurations             : Configurations_Access := Null;
      Loaded                     : Boolean := False;
      Modified                   : Boolean := False;
      Path                       : Ada_Lib.Strings.Unlimited.String_Type;
      Presets                    : Presets_Access := Null;
   end record;

   type Setup_Access             is access all Setup_Type;
   type Setup_Constant_Access    is access constant Setup_Type;

   procedure Dump (
      Setup                      : in     Setup_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

   function File_Path
   return String;

   function Get_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Configuration_Type'class
   with Pre => Setup.Has_Configuration (Configuration_ID);

   function Get_Preset (
      Setup                      : in     Setup_Type;
      Preset_Id                  : in     Preset_ID_Type
   ) return Preset_Type'class
   with Pre => Setup.Is_Loaded;

   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Preset_ID_Type;

   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Configuration_Id           : in     Configuration_ID_Type
   ) return Preset_ID_Type;

   function Has_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Boolean
   with Pre => Setup.Is_Loaded;

   function Has_Preset (
      Setup                      : in     Setup_Type;
      Preset_Id                  : in     Preset_ID_Type
   ) return Boolean
   with Pre => Setup.Is_Loaded;

   function Is_Loaded (
      Setup                      : in     Setup_Type
   ) return Boolean;

   procedure Load  (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Name                       : in     String
   ) with Pre => not Setup.Is_Loaded,
          Post => Setup.Is_Loaded;

   function Make_Image_Name (
      Row                        : in     Configuration.Camera.Row_Type;
      Column                     : in     Configuration.Camera.Column_Type
   ) return String;

   function Configuration_Label (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return String
   with Pre => Setup.Has_Configuration (Configuration_ID);

   function Configuration_Preset (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Preset_ID_Type
   with Pre => Setup.Has_Configuration (Configuration_ID);

   function Preset_Column (
      Setup                      : in     Setup_Type;
      Preset                     : in     Preset_ID_Type
   ) return Column_Type
   with Pre => Setup.Has_Preset (Preset);

   function Preset_Row (
      Setup                      : in     Setup_Type;
      Preset                     : in     Preset_ID_Type
   ) return Row_Type
   with Pre => Setup.Is_Loaded;

   procedure Set_Path (
      Setup                      : in out Setup_Type;
      Path                       : in     String);

   procedure Unload (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Save_Changes               : in     Boolean
   ) with Pre => State.Is_Loaded and then
                 Setup.Is_Loaded;

   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Label                      : in     String);

   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Preset_ID                  : in     Preset_ID_Type);

   procedure Update_Preset (
      Setup                      : in out Setup_Type;
      Preset_ID                  : in     Preset_ID_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type);

   procedure Update  (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class
   ) with Pre => State.Is_Loaded;

   Default_Setup                 : constant String := "setup.cfg";

   Global_Camera_Setup           : Setup_Access := Null;
   Null_Configuration            : constant Configuration_Type := (
      Configuration_ID  => Configuration_Not_Set,
      Label             => Ada_Lib.Strings.Unlimited.Null_String,
      Preset_ID         => Preset_Not_Set,
      Loaded            => False,
      Updated           => False);

   Null_Preset                      : constant Preset_Type := (
      Column            => Column_Not_Set,
      Preset_ID         => Preset_Not_Set,
      Row               => Row_Not_Set,
      Loaded            => False,
      Updated           => False);


end Configuration.Camera.Setup;
