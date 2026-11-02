with Ada_Lib.Configuration;
with Ada_Lib.Strings.Unlimited;
with Video.Lib;

package Configuration.Configuration is

   type Configuration_ID_Type is new
                                 Base_Configuration_ID_Type;

   type Configuration_Type    is new Abstract_Configuration_Type
                                  with private;
   type Configuration_Class_Access
                              is access all Configuration_Type'class;
   type Configuration_Constant_Access
                              is access Constant Configuration_Type;
   type Configuration_Constant_Class_Access
                              is access Constant Configuration_Type'class;

   procedure Dump (
      Setup          : in     Configuration_Type;
      What           : in     String := "";
      From           : in     String := Ada_Lib.Trace.Here);

   procedure Create (
      Configuration_ID        : in     Configuration_ID_Type;
      Label                   : in     String;
      Preset_ID               : in     Video.Lib.Preset_ID_Type);

   function Get_Configuration (
      Configuration_ID        : in     Configuration_ID_Type
   ) return Configuration_Constant_Access;

   function Get_Label (
      Configuration_ID        : in     Configuration_ID_Type
   ) return String;

   function Get_Preset_ID (
      Configuration_ID        : in     Configuration_ID_Type
   ) return Video.Lib.Preset_ID_Type;

   procedure Update (
      Configuration_File      : in out Ada_Lib.Configuration.Configuration_Type);

   procedure Update_Label (
      Configuration_ID           : in     Configuration_ID_Type;
      Label                      : in     String);

   procedure Update_Preset_ID (
      Configuration_ID           : in     Configuration_ID_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type);

   Configuration_Not_Set           : constant Configuration_ID_Type :=
                                 Configuration_ID_Type'last;
private

   type Configuration_Type    is new Abstract_Configuration_Type with record
      Label                   : Ada_Lib.Strings.Unlimited.String_Type;
      Preset_ID               : Video.Lib.Preset_ID_Type;
   end record;

end Configuration.Configuration;

