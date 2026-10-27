with Ada.Text_IO;
--with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Interfaces;

package body Video.Preset is

   use type Configuration.Column_Type;
   use type Configuration.Row_Type;
-- use type Interfaces.Unsigned_64;

   Debug             : Boolean renames Lib.Video_Options.Presets_Debug;

   Standard_Presets  : Stanard_Presets_Array := (others => Preset_ID_Not_Set);

   Presets                 : Presets_Type;

   ---------------------------------------------------------------
   procedure Allocate_Presets (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Maximum_Presets      : in     Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
      Presets.Camera_Presets (Cameras_Index) := new Presets_Array_Type'(
         1 .. Maximum_Presets => Null);
   end Allocate_Presets;

   ---------------------------------------------------------------
   function Constructor (
      Camera_ID   : in     Lib.Camera_ID_Type;
      Preset_ID   : in     Preset_ID_Type;
      Column      : in     Configuration.Column_Type :=
                              Configuration.Column_Not_Set;
      Row         : in     Configuration.Row_Type := Configuration.Row_Not_Set
   ) return Preset_Type is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID" & Preset_ID'img);
      return (Configuration.Configuration_Package.Configuration_Type with
         Camera      => Lib.Get_Camera (Camera_ID),
         Camera_ID   => Camera_ID,
         Column      => Column,
         Preset_ID   => Preset_ID,
         Row         => Row);
   end Constructor;

   ---------------------------------------------------------------
   procedure Dump (
      Preset               : in     Preset_Type;
      What                 : in     String := "";
      From                 : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

      use Ada.Text_IO;

   begin
      if What'length > 0 then
         Put_Line (What);
      end if;
      Put_Line ("Camera ID" & Preset.Camera_ID'img);
      Put_Line ("Preset ID" & Preset.Preset_ID'img);
      Put_Line ("Column" & Preset.Get_Column'img);
      Put_Line ("Row" & Preset.Row'img);
   end Dump;

   -------------------------------------------------------------------------
   procedure Dump_Presets is
   -------------------------------------------------------------------------

   begin
      for Preset in Standard_Presets'range loop
         Ada.Text_IO.Put_Line ("Preset " & Preset'img);
      end loop;
   end Dump_Presets;

   -------------------------------------------------------------------------
   function Get_Camera (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Lib.Camera_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID " & Preset.Image & " from " & From);
      return Preset.Camera;
   end Get_Camera;

   ----------------------------------------------------------------
   function Get_Column (
      Preset               : in     Preset_Type
   ) return Configuration.Column_Type is
   ----------------------------------------------------------------

   begin
      return Preset.Column;
   end Get_Column;

   -------------------------------------------------------------------------
   function Get_Default_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Default_Preset " & Standard_Presets (Default_Preset)'img);
      return Standard_Presets (Default_Preset);
   end Get_Default_Preset_ID;

   -------------------------------------------------------------------------
   function Get_First_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "First_Presett " & Standard_Presets (First_Preset)'img);
      return Standard_Presets (First_Preset);
   end Get_First_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Preset (
      Cameras_Index         : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Preset_Access is
   -------------------------------------------------------------------------

   begin
      return Presets.Camera_Presets (Cameras_Index).all (Preset_ID);
   end Get_Preset;

   -------------------------------------------------------------------------
   function Get_Preset_ID (
      Preset               : in     Preset_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID " & Preset.Preset_ID'img & " from " & From);
      return Preset.Preset_ID;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Camera_Index               : in     Lib.Cameras_Index_Type;
      Row                        : in     Video.Configuration.Row_Type;
      Column                     : in     Video.Configuration.Column_Type
   ) return Preset_ID_Type is
   ----------------------------------------------------------------

      Camera_Presets             : Presets_Array_Type renames
                                    Presets.Camera_Presets (Camera_Index).all;
   begin
      Log_In (Debug, "row" & Row'img & " column" & Column'img);
      for Preset_Pointer of Camera_Presets loop
         declare
            Preset               : Preset_Type renames Preset_Pointer.all;

         begin
            if    Preset.Get_Row = Row and then
                  Preset.Get_Column = Column then
               declare
                  Result            : constant Preset_ID_Type :=
                                       Preset.Get_Preset_ID;
               begin
                  Log_Out (Debug, "preset" & Result'img &
                      " row" & Row'img & " column" & Column'img);
                  return Result;
               end;
            end if;
         end;
      end loop;

      Log_Out (Debug, "not defined");
      return Video.Preset.Preset_ID_Not_Set;
   end Get_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Last_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Last preset " & Standard_Presets (Last_Preset)'img);
      return Standard_Presets (Last_Preset);
   end Get_Last_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Power_On_Preset_ID
   return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Power_On_Preset " & Standard_Presets (Power_On_Preset)'img);
      return Standard_Presets (Power_On_Preset);
   end Get_Power_On_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Standard_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Which_Preset " & Which_Preset'img &
         " Preset_ID " & Standard_Presets (Which_Preset)'img);
      return Standard_Presets (Which_Preset);
   end Get_Standard_Preset;

   ----------------------------------------------------------------
   function Get_Row (
      Preset               : in     Preset_Type
   ) return Configuration.Row_Type is
   ----------------------------------------------------------------

   begin
      return Preset.Row;
   end Get_Row;

   -------------------------------------------------------------------------
   function Has_Camera (
      Preset                  : in     Preset_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Preset.Camera /= Null and then
             Preset.Camera_ID /= Lib.Camera_ID_Not_Set;
   end Has_Camera;

   -------------------------------------------------------------------------
   function Has_Preset (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Presets.Camera_Presets (Cameras_Index) /= Null and then
             Presets.Camera_Presets (Cameras_Index) (Preset_ID) /= Null;
   end Has_Preset;

   -------------------------------------------------------------------------
   function Has_Presets (
      Cameras_Index         : in     Lib.Cameras_Index_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Presets.Camera_Presets (Cameras_Index) /= Null;
   end Has_Presets;

   -------------------------------------------------------------------------
   function Have_Preset (
      Which_Preset               : in     Which_Preset_Type
   ) return Boolean is
   -------------------------------------------------------------------------

      Result   : constant Boolean := Standard_Presets (Which_Preset) /=
                  Preset_ID_Not_Set;

   begin
      return Log_Here (Result, Trace_Pre_Post (Result, Debug),
         "Which_Preset " & Which_Preset'img &
         " Preset_ID " & Standard_Presets (Which_Preset)'img);
   end Have_Preset;

   ---------------------------------------------------------------
   function Image (
      Preset               : in     Preset_Type
   ) return String is
   ---------------------------------------------------------------

   begin
      return "preset id" & Preset.Preset_ID'img &
         " camera id" & Preset.Camera_ID'img &
         " column" & Preset.Column'img &
         " row" & Preset.Row'img;
   end Image;

   -------------------------------------------------------------------------
   function Is_Set (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

      Result   : constant Boolean :=
                  Preset.Preset_ID /= Preset_ID_Not_Set and then
                  Preset.Camera_ID /= Lib.Camera_ID_Not_Set and then
                  Preset.Column /= Configuration.Column_Not_Set and then
                  Preset.Row /= Configuration.Row_Not_Set;

   begin
      return Log_Here (Result, Debug,
         "Preset " & Preset.Image & " from " & From);
   end Is_Set;

   ----------------------------------------------------------------
   function Is_Valid (
      Camera_ID            : in     Lib.Camera_ID_Type;
      Preset_ID            : in     Preset_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Preset_ID <= Presets.Camera_Presets (Camera_ID).all'last;
   end Is_Valid;

   ----------------------------------------------------------------
   function Make_Image_Name (
      Preset                     : in     Preset_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Ada_Lib.Strings.Trim (Preset.Row'img) & ":" &
         Ada_Lib.Strings.Trim (Preset.Column'img);
   end Make_Image_Name;

   -------------------------------------------------------------------------
   procedure Set_Default_Preset_ID (
      Preset_ID                  : in     Preset_ID_Type) is
   -------------------------------------------------------------------------

   begin
      Set_Preset_ID (Default_Preset, Preset_ID);
   end Set_Default_Preset_ID;

   ----------------------------------------------------------------------------
   procedure Set_Preset (
      Cameras_Index        : in     Lib.Cameras_Index_Type;
      Preset_ID            : in     Preset_ID_Type;
      Preset               : in     Preset_Access) is
   ----------------------------------------------------------------------------

   begin
      Presets.Camera_Presets (Cameras_Index) (Preset_ID) := Preset;
   end Set_Preset;

   ----------------------------------------------------------------------------
   procedure Set_Preset_ID (
      Which_Preset               : in     Which_Preset_Type;
      Preset_ID                  : in     Preset_ID_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "which " & Which_Preset'img & " " & Preset_ID'img);
      Standard_Presets (Which_Preset) := Preset_ID;
      Log_Out (Debug);
   end Set_Preset_ID;

end Video.Preset;
