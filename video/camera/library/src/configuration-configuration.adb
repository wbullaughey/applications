with Ada_Lib.Trace; use Ada_Lib.Trace;
with Generics;

package body Configuration.Configuration is

   type Configurations_Type   is array (Configuration_ID_Type range <>) of
                                 Configuration_Class_Access;
   type Configurations_Access is access all Configurations_Type;

   type Match_Type            is null record;

   package Configuration_Package
                              is new Generics.Vector_Package (
      ID_Type                 => Configuration_ID_Type,
      Object_Type             => Configuration_Type,
      Object_Class_Access     => Configuration_Class_Access,
      Object_Constant_Class_Access
                              => Configuration_Constant_Class_Access,
      Match_Type              => Match_Type,
      Vector_Type             => Configurations_Type,
      Vectors_Type            => Configurations_Access);

   ---------------------------------------------------------------
   procedure Create (
      Configuration_ID        : in     Configuration_ID_Type;
      Label                   : in     String;
      Preset_ID               : in     Video.Lib.Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
not_implemented;
   end Create;

   ---------------------------------------------------------------
   procedure Dump (
      Setup          : in     Configuration_Type;
      What           : in     String := "";
      From           : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

   begin
not_implemented;
   end Dump;

   ---------------------------------------------------------------
   function Get_Configuration (
      Configuration_ID        : in     Configuration_ID_Type
   ) return Configuration_Constant_Access is
   ---------------------------------------------------------------

   begin
not_implemented;
return null;
   end Get_Configuration;

   ---------------------------------------------------------------
   function Get_Label (
      Configuration_ID        : in     Configuration_ID_Type
   ) return String is
   ---------------------------------------------------------------

   begin
not_implemented;
return "";
   end Get_Label;

   ---------------------------------------------------------------
   function Get_Preset_ID (
      Configuration_ID        : in     Configuration_ID_Type
   ) return Video.Lib.Preset_ID_Type is
   ---------------------------------------------------------------

   begin
not_implemented;
return Video.Lib.Preset_ID_Not_Set;
   end Get_Preset_ID;

   ---------------------------------------------------------------
   procedure Update (
      Configuration_File      : in out Ada_Lib.Configuration.Configuration_Type) is
   ---------------------------------------------------------------

   begin
not_implemented;
--          for Configuration_ID in 1 .. Number_Configurations loop
--             declare
--                Name                 : constant String := "configuration_" &
--                                        Trim (Configuration_ID'img);
--                Configuration        : Configuration.Configuration_Type renames
--                                        Configuration.Get_Configuration (Configuration_ID);
--             begin
--                Log_Here (Debug, "configuration" & Configuration_ID'img
--                   & Quote (" name", Name) &
--                   " preset id" & Configuration.Preset_Id'img &
--                   Configuration.Label.Quote (" label"));
--
--                if Configuration.Preset_ID /= Video.Lib.Preset_ID_Not_Set then
--                   Config.Set (Name, Trim (Configuration.Preset_Id'img) & "," &
--                      Configuration.Label.Coerce, Update => False);
--                end if;
--             end;
--          end loop;
   end Update;

   ---------------------------------------------------------------
   procedure Update_Label (
      Configuration_ID           : in     Configuration_ID_Type;
      Label                      : in     String) is
   ---------------------------------------------------------------

   begin
not_implemented;
   end Update_Label;

   ---------------------------------------------------------------
   procedure Update_Preset_ID (
      Configuration_ID           : in     Configuration_ID_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
not_implemented;
   end Update_Preset_ID;

end Configuration.Configuration;

