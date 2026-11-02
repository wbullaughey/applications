with Ada_Lib.Configuration;
--with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;
with Camera.Commands;
with Configuration.Camera; -- .State;
--with Video.Generic_Package;
with Video.Lib;

package Camera.Preset is

   use type Commands.Camera_Class_Access;
-- use type Video.Lib.Camera_Class_Access;
   use type Video.Lib.Camera_ID_Type;
   use type Video.Lib.Preset_ID_Type;

   Failed                        : exception;

   type Which_Preset_Type        is (
                                    Default_Preset,
                                    First_Preset,
                                    Last_Preset,
                                    Maximum_Preset,
                                    Power_On_Preset);

   function Get_Default_Preset_ID (
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type
   with Pre => Have_Preset (Camera_Index, Default_Preset);

   function Get_First_Preset_ID(
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type
   with Pre => Have_Preset (Camera_Index, First_Preset);

   function Get_Last_Preset_ID (
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type;
-- with Pre => Have_Preset (Camera_Index);

   function Get_Power_On_Preset_ID(
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type
   with Pre => Have_Preset (Camera_Index, Power_On_Preset);

   function Get_Preset_ID (   -- ok to return Camera_ID_Not_Set
      Camera_Index         : in     Video.Lib.Camera_Index_Type;
      Row                  : in     Configuration.Row_Type;
      Column               : in     Configuration.Column_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video.Lib.Preset_ID_Type;
-- with Pre => Has_Preset (Camera_Index, Row, Column);

   function Get_Standard_Preset (
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Which_Preset         : in     Which_Preset_Type
   ) return Video.Lib.Preset_ID_Type
   with Pre => Have_Preset (Camera_Index, Which_Preset);

   function Has_Preset (
      Preset_ID            : in     Video.Lib.Preset_ID_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean;
--
   function Have_Preset (
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Which_Preset         : in     Which_Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

-- function Is_Valid (
--    Preset_ID            : in     Video.Lib.Preset_ID_Type;
--    From                 : in     String := Ada_Lib.Trace.Here
-- ) return Boolean;

   procedure Set_Default_Preset_ID (
      Camera_Index               : in     Video.Lib.Camera_Index_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type);

   procedure Set_Last_Preset (
      Camera_Index               : in     Video.Lib.Camera_Index_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type);

   procedure Set_Preset_ID (
      Camera_Index               : in     Video.Lib.Camera_Index_Type;
      Which_Preset               : in     Which_Preset_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type);

   type Preset_Type              is new Video.Lib.Object_Type with private;
   type Preset_Access            is access all Preset_Type;
   type Preset_Class_Access      is access all Preset_Type'class;
   type Preset_Constant_Access   is access constant Preset_Type;
   type Preset_Constant_Class_Access
                                 is access constant Preset_Type'class;
   type Presets_Type             is array (Video.Lib.Preset_ID_Type range <>)
                                    of Preset_Class_Access;
   type Presets_Access           is access all Presets_Type;
   type Stanard_Presets_Array    is array (
                                    Video.Lib.Camera_Index_Type,
                                    Which_Preset_Type) of
                                       Video.Lib.Preset_ID_Type;
  overriding
  procedure Dump (
     Preset               : in     Preset_Type;
     What                 : in     String := "";
     From                 : in     String := Ada_Lib.Trace.Here);
--
  function Get_Camera (
     Preset               : in     Preset_Type;
     From                 : in     String := Ada_Lib.Trace.Here
  ) return Commands.Camera_Class_Access
  with Pre => Preset.Has_Camera;
--
  function Get_Camera_ID (
     Preset               : in     Preset_Type;
     From                 : in     String := Ada_Lib.Trace.Here
  ) return Video.Lib.Camera_ID_Type
  with Pre => Preset.Has_Camera;
--
  function Get_Column (
     Preset               : in     Preset_Type
  ) return Configuration.Column_Type
  with Pre => Preset.Is_Set;

  function Get_Constant_Preset (
     Preset_ID             : in     Video.Lib.Preset_ID_Type;
     From                  : in     String := Ada_Lib.Trace.Here
  ) return Preset_Constant_Access
  with Pre => Has_Preset (Preset_ID);

  function Get_Preset (
     Preset_ID             : in     Video.Lib.Preset_ID_Type;
     From                  : in     String := Ada_Lib.Trace.Here
  ) return Preset_Access
  with Pre => Has_Preset (Preset_ID);
--
  function Get_Preset_ID (
     Preset               : in     Preset_Type;
     From                 : in     String := Ada_Lib.Trace.Here
  ) return Video.Lib.Preset_ID_Type;
--
  function Get_Row (
     Preset               : in     Preset_Type
  ) return Configuration.Row_Type;
--      with Pre => Preset.Is_Set;
--
  function Has_Camera (
     Preset               : in     Preset_Type;
     From                 : in     String := Ada_Lib.Trace.Here
  ) return Boolean;
--
   function Has_Preset (
      Camera_Index            : in     Video.Lib.Camera_Index_Type;
      Row                  : in     Configuration.Row_Type;
      Column               : in     Configuration.Column_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   function Image (
      Preset                     : in     Preset_Type
   ) return String;

   function Is_Set (
     Preset               : in     Preset_Type;
     From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean;
--
--   -- procedure Load_Preset (
--   --    Path                 : in     String;
--   --    Camera_ID            : in     Video.Lib.Camera_Index_Type;
--   --
--   --    Preset               :    out Prefix_Type)'
--   --
  function Make_Image_Name (
     Preset               : in     Preset_Type
  ) return String;

   procedure Update (
      Preset      : in out Preset_Type;
      Camera      : in     Commands.Camera_Class_Access;
      Camera_Index: in     Video.Lib.Camera_ID_Type;
      Column      : in     Configuration.Column_Type :=
                              Configuration.Camera.Column_Not_Set;
      Preset_ID   : in     Video.Lib.Preset_ID_Type;
      Row         : in     Configuration.Row_Type := Configuration.Camera.Row_Not_Set
   ) with Pre    => Camera_Index /= Video.Lib.Camera_ID_Not_Set and then
                    Camera /= Null and then
                    Preset_ID /= Video.Lib.Preset_ID_Not_Set;

   procedure Update (
      Configuration_File      : in out Ada_Lib.Configuration.Configuration_Type);

   procedure Allocate_Presets (
      Maximum_Presets          : in     Video.Lib.Preset_ID_Type);

   procedure Create_Preset (
      Camera      : in     Commands.Camera_Class_Access;
      Camera_Index: in     Video.Lib.Camera_ID_Type;
      Column      : in     Configuration.Column_Type :=
                              Configuration.Camera.Column_Not_Set;
      Preset_ID   : in     Video.Lib.Preset_ID_Type;
      Row         : in     Configuration.Row_Type :=
                              Configuration.Camera.Row_Not_Set);
   function Image (
      Preset_ID                   : in     Video.Lib.Preset_ID_Type
   ) return String;

private

   type Preset_Type  is new Video.Lib.Object_Type with record
      Camera         : Commands.Camera_Class_Access := Null;
      Camera_ID      : Video.Lib.Camera_ID_Type := Video.Lib.Camera_ID_Not_Set;
      Column         : Configuration.Column_Type :=
                        Configuration.Camera.Column_Not_Set;
      Preset_ID      : Video.Lib.Preset_ID_Type := Video.Lib.Preset_ID_Not_Set;
      Row            : Configuration.Row_Type := Configuration.Camera.Row_Not_Set;
   end record;

end Camera.Preset;
