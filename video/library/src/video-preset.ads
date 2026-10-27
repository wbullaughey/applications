--with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;
with Video.Configuration;
with Video.Lib;

package Video.Preset is

   use type Lib.Camera_Class_Access;
   use type Lib.Camera_ID_Type;

   type Which_Preset_Type        is (
                                    Default_Preset,
                                    First_Preset,
                                    Last_Preset,
                                    Maximum_Preset,
                                    Power_On_Preset);

   type Preset_ID_Type           is new Natural;
   Preset_ID_Not_Set             : constant Preset_ID_Type :=
                                    Preset_ID_Type'last;

   function Get_Default_Preset_ID
   return Preset_ID_Type
   with Pre => Have_Preset (Default_Preset);

   function Get_First_Preset_ID
   return Preset_ID_Type
   with Pre => Have_Preset (First_Preset);

   function Get_Last_Preset_ID
   return Preset_ID_Type;
-- with Pre => Have_Preset (Last_Preset);

   function Get_Power_On_Preset_ID
   return Preset_ID_Type
   with Pre => Have_Preset (Power_On_Preset);

   function Get_Standard_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Preset_ID_Type
   with Pre => Have_Preset (Which_Preset);

   procedure Dump_Presets;

   function Have_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Boolean;

   procedure Set_Default_Preset_ID (
      Preset_ID                  : in     Preset_ID_Type);

   procedure Set_Preset_ID (
      Which_Preset               : in     Which_Preset_Type;
      Preset_ID                  : in     Preset_ID_Type);

   type Stanard_Presets_Array    is array (Which_Preset_Type) of
                                    Preset_ID_Type;
-- type Standard_Presets_Access  is access all Stanard_Presets_Array;
--
   type Preset_Type
      is new Configuration.Configuration_Package.Configuration_Type with
         private;

   type Preset_Access      is access all Preset_Type;

   procedure Allocate_Presets (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Maximum_Presets      : in     Preset_ID_Type);

   function Constructor (
      Camera_ID   : in     Lib.Camera_ID_Type;
      Preset_ID   : in     Preset_ID_Type;
      Column      : in     Configuration.Column_Type :=
                              Configuration.Column_Not_Set;
      Row         : in     Configuration.Row_Type := Configuration.Row_Not_Set
   ) return Preset_Type
   with Pre    => Camera_ID /= Lib.Camera_ID_Not_Set and then
                  Lib.Has_Camera (Camera_ID);

   procedure Dump (
      Preset               : in     Preset_Type;
      What                 : in     String := "";
      From                 : in     String := Ada_Lib.Trace.Here);

   function Get_Camera (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Lib.Camera_Class_Access
   with Pre => Preset.Has_Camera;

   function Get_Column (
      Preset               : in     Preset_Type
   ) return Configuration.Column_Type
   with Pre => Preset.Is_Set;

   function Get_Preset_ID (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Preset_ID_Type;

   function Get_Row (
      Preset               : in     Preset_Type
   ) return Configuration.Row_Type
   with Pre => Preset.Is_Set;

   function Has_Camera (
      Preset               : in     Preset_Type
   ) return Boolean;

   function Image (
      Preset                     : in     Preset_Type
   ) return String;

   function Is_Set (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   function Make_Image_Name (
      Preset               : in     Preset_Type
   ) return String;

   type Presets_Type       is tagged private;

   type Presets_Access     is access Presets_Type;

   function Get_Preset (
      Cameras_Index         : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Preset_Access
   with Pre    => Has_Preset (Cameras_Index, Preset_ID),
        Post   => Get_Preset'result /= Null;

   function Get_Preset_ID (
      Camera_Index               : in     Lib.Cameras_Index_Type;
      Row                        : in     Video.Configuration.Row_Type;
      Column                     : in     Video.Configuration.Column_Type
   ) return Preset_ID_Type;

   function Has_Preset (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Boolean;

   function Has_Presets (
      Cameras_Index        : in     Lib.Cameras_Index_Type
   ) return Boolean;

   function Is_Valid (
      Camera_ID            : in     Lib.Camera_ID_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Boolean;

   procedure Set_Preset (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type;
      Preset               : in     Preset_Access
   ) with Pre => Preset /= Null and then
                 Has_Presets (Cameras_Index);

   Null_Preset                   : constant Preset_Type;

private
   type Preset_Type  is new Configuration.Configuration_Package.
                        Configuration_Type with record
      Camera         : Lib.Camera_Class_Access := Null;
      Camera_ID      : Lib.Camera_ID_Type := Lib.Camera_ID_Not_Set;
      Column         : Configuration.Column_Type :=
                        Configuration.Column_Not_Set;
      Preset_ID      : Preset_ID_Type := Preset_ID_Not_Set;
      Row            : Configuration.Row_Type := Configuration.Row_Not_Set;
   end record;

   type Presets_Array_Type
      is array (Preset_ID_Type range <>) of Preset_Access;

   type Presets_Array_Access
      is access all Presets_Array_Type;

   type Camera_Presets_Type
      is array (Lib.Cameras_Index_Type) of Presets_Array_Access;

   type Presets_Type       is tagged record
      Camera_Presets       : Camera_Presets_Type;
   end record;

   Null_Preset       : constant Preset_Type := (
                        Configuration.Configuration_Package.Configuration_Type
                           with
      Camera      => Null,
      Camera_ID   => Lib.Camera_ID_Not_Set,
      Column      => Configuration.Column_Not_Set,
      Preset_ID   => Preset_ID_Not_Set,
      Row         => Configuration.Row_Not_Set);

end Video.Preset;
