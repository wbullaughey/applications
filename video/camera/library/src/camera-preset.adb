with Ada.Text_IO;
--with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Generics;
--with Interfaces;

package body Camera.Preset is

   use type Configuration.Column_Type;
   use type Configuration.Row_Type;

   type Match_Type         is record
      Camera_Index         : Video.Lib.Camera_Index_Type;
      Column               : Configuration.Column_Type;
      Row                  : Configuration.Row_Type;
   end record;

   package Presets_Package is new Generics.Vector_Package (
      ID_Type               => Video.Lib.Preset_ID_Type,
      Match_Type            => Match_Type,
      Object_Type           => Preset_Type,
      Object_Class_Access   => Preset_Class_Access,
      Object_Constant_Class_Access
                            => Preset_Constant_Class_Access,
      Vector_Type           => Presets_Type,
      Vectors_Type          => Presets_Access);

   Debug             : Boolean renames Video.Lib.Video_Options.Presets_Debug;

   Standard_Presets  : Stanard_Presets_Array := (others => (
                        others =>Video.Lib.Preset_ID_Not_Set));

   ---------------------------------------------------------------
   procedure Allocate_Presets (
      Maximum_Presets       : in     Video.Lib.Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug, "max presets" & Maximum_Presets'img);
      Presets_Package.Allocate_Vector (Maximum_Presets);
   end Allocate_Presets;
--
   ---------------------------------------------------------------
   function Match_Preset (
      Preset                  : in     Preset_Constant_Class_Access;
      Match                   : in     Match_Type
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Log_Here (
            Preset.Camera_ID = Match.Camera_Index and then
            Preset.Column = Match.Column and then
            Preset.Column = Match.Column, Debug);
   end Match_Preset;

   ---------------------------------------------------------------
   procedure Create_Preset (
      Camera         : in     Commands.Camera_Class_Access;
      Camera_Index   : in     Video.Lib.Camera_ID_Type;
      Column         : in     Configuration.Column_Type :=
                                 Configuration.Camera.Column_Not_Set;
      Preset_ID      : in     Video.Lib.Preset_ID_Type;
      Row            : in     Configuration.Row_Type :=
                                 Configuration.Camera.Row_Not_Set) is
   ---------------------------------------------------------------

      Preset      : constant Preset_Access := new Preset_Type;

   begin
      Log_In (Debug, "camera " & Camera_Index'img & " preset" & Preset_ID'img &
         " column" & Column'img & " row" & Row'img);
      Update (Preset.all, Camera, Camera_Index, Column, Preset_ID, Row);
      Presets_Package.Set_Object (Preset_ID, Preset_Class_Access (Preset));
      Log_Out (Debug);
   end Create_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Dump (
      Preset               : in     Preset_Type;
      What                 : in     String := "";
      From                 : in     String := Ada_Lib.Trace.Here) is
   ---------------------------------------------------------------

      use Ada.Text_IO;

   begin
      if What'length > 0 then
         Put (What & " ");
      end if;
      Put_Line ("from " & From);
      Put_Line ("Camera ID" & Preset.Camera_ID'img);
      Put_Line ("Preset ID" & Preset.Preset_ID'img);
      Put_Line ("Column" & Preset.Get_Column'img);
      Put_Line ("Row" & Preset.Row'img);
   end Dump;

   ---------------------------------------------------------------------------
   function Find_Preset (
      Camera_Index            : in     Video.Lib.Camera_Index_Type;
      Column               : in     Configuration.Column_Type;
      Row                  : in     Configuration.Row_Type
   ) return Preset_Access is
   ---------------------------------------------------------------------------

      Match                : constant Match_Type := (
                              Camera_Index   => Camera_Index,
                              Column         => Column,
                              Row            => Row);
   begin
      return Preset_Access (Presets_Package.Iterate (Match_Preset'access, Match));
   end Find_Preset;

   ---------------------------------------------------------------------------
   function Get_Camera (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Commands.Camera_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID " & Preset.Image & " from " & From);
      return Preset.Camera;
   end Get_Camera;

   -------------------------------------------------------------------------
   function Get_Camera_ID (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video.Lib.Camera_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "PCamera_ID " & Preset.Camera_ID'img & " " & From);
      return Preset.Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------
   function Get_Column (
      Preset               : in     Preset_Type
   ) return Configuration.Column_Type is
   ----------------------------------------------------------------

   begin
      return Preset.Column;
   end Get_Column;

   -------------------------------------------------------------------------
  function Get_Constant_Preset (
     Preset_ID             : in     Video.Lib.Preset_ID_Type;
     From                  : in     String := Ada_Lib.Trace.Here
  ) return Preset_Constant_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "preset id" & Preset_ID'img & " from " & From);
      return Preset_Constant_Access (Presets_Package.Get (Preset_ID));
   end Get_Constant_Preset;

   -------------------------------------------------------------------------
   function Get_Default_Preset_ID (
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "camera" & Camera_Index'img &
         " Default_Preset " & Standard_Presets (
         Camera_Index, Default_Preset)'img);
      return Standard_Presets (Camera_Index, Default_Preset);
   end Get_Default_Preset_ID;

   -------------------------------------------------------------------------
   function Get_First_Preset_ID(
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "First_Presett " & Standard_Presets (
         Camera_Index, First_Preset)'img);
      return Standard_Presets (Camera_Index, First_Preset);
   end Get_First_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Last_Preset_ID(
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Last preset " & Standard_Presets (
         Camera_Index, Last_Preset)'img);
      return Standard_Presets (Camera_Index, Last_Preset);
   end Get_Last_Preset_ID;

   -------------------------------------------------------------------------
   function Get_Power_On_Preset_ID(
      Camera_Index        : in     Video.Lib.Camera_Index_Type
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "camera" & Camera_Index'img & " Power_On_Preset " &
         Standard_Presets (Camera_Index, Power_On_Preset)'img);
      return Standard_Presets (Camera_Index, Power_On_Preset);
   end Get_Power_On_Preset_ID;

   -------------------------------------------------------------------------
  function Get_Preset (
     Preset_ID             : in     Video.Lib.Preset_ID_Type;
     From                  : in     String := Ada_Lib.Trace.Here
  ) return Preset_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "preset id" & Preset_ID'img & " from " & From);
      return Preset_Access (Presets_Package.Get (Preset_ID));
   end Get_Preset;

   -------------------------------------------------------------------------
   function Get_Preset_ID (
      Preset               : in     Preset_Type;
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_ID " & Preset.Preset_ID'img & " from " & From);
      return Preset.Preset_ID;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Camera_Index              : in     Video.Lib.Camera_Index_Type;
      Row                        : in     Configuration.Row_Type;
      Column                     : in     Configuration.Column_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Video.Lib.Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "camera index" & Camera_Index'img &
         " row" & Row'img & " column" & Column'img &
         " from " & From);

      declare
         Preset            : constant Preset_Access :=
                              Find_Preset (Camera_Index, Column, Row);
      begin
         return Preset.Preset_ID;
      end;

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Row (
      Preset               : in     Preset_Type
   ) return Configuration.Row_Type is
   ----------------------------------------------------------------

   begin
      return Preset.Row;
   end Get_Row;

   -------------------------------------------------------------------------
   function Get_Standard_Preset (
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Which_Preset               : in     Which_Preset_Type
   ) return Video.Lib.Preset_ID_Type is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug, "camera" & Camera_Index'img &
         " Which_Preset " & Which_Preset'img &
         " Preset_ID " & Standard_Presets (Camera_Index, Which_Preset)'img);
      return Standard_Presets (Camera_Index, Which_Preset);
   end Get_Standard_Preset;

   -------------------------------------------------------------------------
   function Has_Camera (
      Preset                  : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Log_Pre_Post (
         Preset.Camera /= Null and then
         Preset.Camera_ID /= Video.Lib.Camera_ID_Not_Set, Debug,
         "from " & From);
   end Has_Camera;

   ---------------------------------------------------------------
  function Has_Preset (
     Preset_ID             : in     Video.Lib.Preset_ID_Type;
      From                 : in     String := Ada_Lib.Trace.Here
  ) return Boolean is
   ---------------------------------------------------------------

   begin
      return Log_Here (Presets_Package.Has_Object (Preset_ID), Debug,
         "preset id" & Preset_ID'img & " from " & From);
   end Has_Preset;

   -------------------------------------------------------------------------
   function Has_Preset (
      Camera_Index            : in     Video.Lib.Camera_Index_Type;
      Row                  : in     Configuration.Row_Type;
      Column               : in     Configuration.Column_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

      Log_It   : constant Boolean := Debug or else
                  Trace_Pre_Post_Conditions or else Trace_Pre_Post_False;
   begin
      Log_In (Log_It, "camera index" & Camera_Index'img &
         " row" & Row'img & " column" & Column'img & " from " & From);
      declare
         Preset            : constant Preset_Access :=
                              Find_Preset (Camera_Index, Column, Row);
      begin
         return Preset /= Null;
      end;

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

 end Has_Preset;

-- -------------------------------------------------------------------------
-- function Has_Presets (
--    Camera_Index         : in     Video.Lib.Camera_Index_Type;
--    From                 : in     String := Ada_Lib.Trace.Here
-- ) return Boolean is
-- -------------------------------------------------------------------------
--
-- begin
--    return Log_Pre_Post (
--       Presets.Camera_Presets (Camera_Index) /= Null,
--       Debug, "from " & From);
-- end Has_Presets;
--
   -------------------------------------------------------------------------
   function Have_Preset (
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Which_Preset         : in     Which_Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Log_Pre_Post (
         Standard_Presets (Camera_Index, Which_Preset) /=
         Video.Lib.Preset_ID_Not_Set, Debug,
         "camera" & Camera_Index'img &
         " Which_Preset " & Which_Preset'img &
         " Preset_ID " & Standard_Presets (Camera_Index, Which_Preset)'img &
         " from " & From);
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
   function Image (
      Preset_ID            : in     Video.Lib.Preset_ID_Type
   ) return String is
   -------------------------------------------------------------------------

   begin
      return Preset_Class_Access (
         Presets_Package.Get (Preset_ID)).Image;
   end Image;

-- ----------------------------------------------------------------
-- function Is_Valid (
--    Preset_ID            : in     Video.Lib.Preset_ID_Type;
--    From                 : in     String := Ada_Lib.Trace.Here
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Log_Here (Preset_ID <= Preset_Package.Get_Vector_Size, Debug,
--       "from " & From);
-- end Is_Valid;

   -------------------------------------------------------------------------
   function Is_Set (
      Preset               : in     Preset_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

      Result   : constant Boolean :=
                  Preset.Preset_ID /= Video.Lib.Preset_ID_Not_Set and then
                  Preset.Camera_ID /= Video.Lib.Camera_ID_Not_Set and then
                  Preset.Column /= Configuration.Camera.Column_Not_Set and then
                  Preset.Row /= Configuration.Camera.Row_Not_Set;

   begin
      return Log_Here (Result, Debug,
         "Preset " & Preset.Image & " from " & From);
   end Is_Set;

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
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Preset_ID            : in     Video.Lib.Preset_ID_Type) is
   -------------------------------------------------------------------------

   begin
      Set_Preset_ID (Camera_Index, Default_Preset, Preset_ID);
   end Set_Default_Preset_ID;

   ----------------------------------------------------------------------------
   procedure Set_Last_Preset (
      Camera_Index               : in     Video.Lib.Camera_Index_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type) is
   ----------------------------------------------------------------------------

   begin
      Standard_Presets (Camera_Index, Last_Preset) := Preset_ID;
   end Set_Last_Preset;

   ----------------------------------------------------------------------------
   procedure Set_Preset_ID (
      Camera_Index        : in     Video.Lib.Camera_Index_Type;
      Which_Preset         : in     Which_Preset_Type;
      Preset_ID            : in     Video.Lib.Preset_ID_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "camera" & Camera_Index'img &
      " which " & Which_Preset'img & " " & Preset_ID'img);
      Standard_Presets (Camera_Index, Which_Preset) := Preset_ID;
      Log_Out (Debug);
   end Set_Preset_ID;

   ---------------------------------------------------------------
   procedure Update (
      Preset      : in out Preset_Type;
      Camera      : in     Commands.Camera_Class_Access;
      Camera_Index: in     Video.Lib.Camera_ID_Type;
      Column      : in     Configuration.Column_Type :=
                              Configuration.Camera.Column_Not_Set;
      Preset_ID   : in     Video.Lib.Preset_ID_Type;
      Row         : in     Configuration.Row_Type := Configuration.Camera.Row_Not_Set) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Preset_ID" & Preset_ID'img);
      Preset.Camera            := Camera;
      Preset.Camera_ID         := Camera_Index;
      Preset.Column            := Column;
      Preset.Preset_ID         := Preset_ID;
      Preset.Row               := Row;
      if Debug then
         Preset.Dump;
      end if;
      Log_Out (Debug);
   end Update;

   ---------------------------------------------------------------
   procedure Update (
      Configuration_File      : in out Ada_Lib.Configuration.Configuration_Type) is
   ---------------------------------------------------------------

   begin
not_implemented;
--          for Preset_Number in Video.Lib.Preset_ID_Type'range loop
--             declare
--                Name        : constant String :=
--                               "preset_" & Trim (Preset_Number'img);
--                Preset      : Standard.Camera.Preset.Preset_Constant_Access renames
--                               Standard.Camera.Preset.Get_Constant_Preset (
--                                  Preset_Number);
--                Column      : constant Column_Type := Preset.Get_Column;
--                Row         : constant Row_Type := Preset.Get_Row;
--
--             begin
--                Log_Here (Debug, "preset" & Preset_Number'img & Quote (" name", Name));
--                if Row /= State.Row_Not_Set then
--                   Config.Set (Name, Trim (Row'img) & "," &
--                      Trim (Column'img), Update => False);
--                end if;
--             end;
--          end loop;
   end Update;

begin
--Debug := True;
--Trace_Options := True;
--Elaborate := True;
   Log_Here (Debug or else Elaborate, "elaborate");

end Camera.Preset;
