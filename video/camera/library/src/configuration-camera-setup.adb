with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Address;
with Ada_Lib.Configuration;
--with Ada_Lib.Options;
with Ada_Lib.Parser;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with ADA_LIB.Strings;use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Options;
--with Camera.Lib.Options.Unit_Test;

package body Configuration.Camera.Setup is

--pragma Elaborate (Ada_Lib.Parser);

   procedure Free is new Ada.Unchecked_Deallocation (
      Configurations_Type,
      Configurations_Access);

   procedure Free is new Ada.Unchecked_Deallocation (
      Presets_Type,
      Presets_Access);

   Debug    : Boolean renames Standard.Camera.Lib.Options.
               Configuration_Options.Setup_Debug;

   ----------------------------------------------------------------
   function Configuration_Label (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Setup.Configurations (Configuration_ID).Label.Coerce;
   end Configuration_Label;

   ----------------------------------------------------------------
   function Configuration_Preset (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Standard.Camera.Preset_ID_Type is
   ----------------------------------------------------------------

      Result   : constant Standard.Camera.Preset_ID_Type :=
                  Setup.Configurations (Configuration_ID).Preset_ID;
   begin
      Log_Here (Debug, "Configuration_ID" & Configuration_ID'img &
         " preset id" & Result'img);
      return Result;
   end Configuration_Preset;

   ----------------------------------------------------------------
   procedure Dump (
      Setup                      : in     Setup_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_LIne ("dump " & (if What'length > 0 then
            "what " & What & " "
         else
            "") &
         "setup from " & From);
      for Configuration in Setup.Configurations.all'range loop
         Put_Line ("Configuration" & Configuration'img);
         Setup.Configurations (Configuration).Dump;
      end loop;
      for Preset in Setup.Presets.all'range loop
         Put_Line ("Preset" & Preset'img);
         Setup.Presets (Preset).Dump;
      end loop;
      Put_Line ("Loaded " & Setup.Loaded'img);
      Put_Line ("Modified " & Setup.Modified'img);
   end Dump;

   ----------------------------------------------------------------
   procedure Dump (
      Configuration              : in     Configuration_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("Configuration "  & (if What'length > 0 then
            "what " & What & " "
         else
            "") &
         Configuration.Configuration_ID'img &
         " from " & From);
      Put_Line (Quote ("  label", Configuration.Label));
      Put_Line ("  Preset ID:" & Configuration.Preset_ID.Image);
   end Dump;

   ----------------------------------------------------------------
   procedure Dump (
      Preset                     : in     Preset_Type;
      What                       : in     String := "";
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ((if What'length > 0 then
            "what " & What & " "
         else
            "") &
         Preset.Preset_ID.Image & " from " & From);
      if Preset.Preset_ID.Is_Set then
         Put_Line ("  Column:" & Preset.Column'img);
         Put_Line ("  Row:" & Preset.Row'img);
      end if;
   end Dump;

-- ----------------------------------------------------------------
-- function File_Path
-- return String is
-- ----------------------------------------------------------------
--
--    Options     : constant Standard.Camera.Lib.Options.
--                   Program_Options_Constant_Class_Access :=
--                      Standard.Camera.Lib.Options.
--                   Program_Options_Constant_Class_Access (
--                      Ada_Lib.Options.Verification.Get_Ada_Lib_Read_Only_Nested_Options);
--    Setup_Path  : Ada_Lib.Strings.Unlimited.String_Type
--                renames Options.Nested_Options.Setup_Path;
-- begin
--    return (if Setup_Path.Length > 0 then
--                Setup_Path.Coerce
--            else
--                Default_Setup);
-- end File_Path;

   ----------------------------------------------------------------
   function Get_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID        : in     Configuration_ID_Type
   ) return Configuration_Type'class is
   ----------------------------------------------------------------

   begin
      return Setup.Configurations (Configuration_ID);
   end Get_Configuration;

   ----------------------------------------------------------------
   function Get_Preset (
      Setup                      : in     Setup_Type;
      Preset_Id                  : in     Standard.Camera.Preset_ID_Type
   ) return Preset_Type'class is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_Id " & Preset_Id.Get_ID'img);
      return (if not Preset_ID.Is_Set then
            Null_Preset
         else
            Setup.Presets (Preset_Id.Get_ID));
   end Get_Preset;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Configuration_Id           : in     Configuration_ID_Type
   ) return Standard.Camera.Preset_ID_Type is
   ----------------------------------------------------------------

      Result                     : constant Standard.Camera.Preset_ID_Type :=
                                    Setup.Configurations (
                                       Configuration_Id).Preset_ID;
   begin
      Log_Here (Debug, "Configuration_Id " & Configuration_Id'img &
         " result " & Result.Image);
      return Result;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Standard.Camera.Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "row" & Row'img & " column" & Column'img);
      for ID in Setup.Presets'first .. Setup.Presets.all'last loop
         declare
            Preset               : Preset_Type renames Setup.Presets (ID);

         begin
            if Preset.Row = Row and then Preset.Column = Column then
               Log_Out (Debug, "preset" & Id'img &
                   " row" & Row'img & " column" & Column'img);
               return Preset.Preset_ID;
            end if;
         end;
      end loop;

      Log_Out (Debug, "not defined");
      return Video.Lib.Null_Preset_ID;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Has_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "configuration id" & Configuration_ID'img);
      if    Configuration_ID <= Setup.Configurations.all'last then
         declare
            Result                     : constant Boolean :=
                                          Setup.Configurations (
                                             Configuration_ID) /=
                                             Null_Configuration;
         begin
            return Log_Out (Result, Debug, "configuration" &
               Configuration_ID'img);
         end;
      else
         Log_Exception (Debug);
         raise Failed with "invalid configuration id" & Configuration_ID'img;
      end if;
   end Has_Configuration;

   ----------------------------------------------------------------
   function Has_Preset (
      Setup                      : in     Setup_Type;
      Preset_Id                  : in     Standard.Camera.Preset_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "preset id " & Preset_ID.Image);

      declare
         Has_Row     : constant Boolean :=
                        Setup.Presets (Preset_Id.Get_ID).Row /= Row_Not_Set;
         Result      : constant Boolean :=
                        Preset_ID.Is_Set and then
                        Has_Row;
      begin
         return Log_Out (Result, Debug, "Is_Set " & Preset_ID.Is_Set'img &
            " Has_Row " & Has_Row'img);
      end;
   end Has_Preset;

-- ----------------------------------------------------------------
-- function Is_Loaded (
--    Setup                      : in     Setup_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Log_Here (Setup.Loaded, Debug,
--       "loaded " & Setup.Loaded'img);
-- end Is_Loaded;

   ----------------------------------------------------------------
   procedure Load (
      Setup       : in out Setup_Type;
      State       : in     Configuration.Camera.State.State_Type'class;
      Name        : in     String) is
   ----------------------------------------------------------------

      Config            : Ada_Lib.Configuration.Configuration_Type;
      Current_Directory : constant String :=
                           Standard.Camera.Lib.Options.Current_Directory;
      Last_Preset_ID    : constant Standard.Camera.Preset_ID_Type :=
                           Video.Lib.Get_Last_Preset_ID;
      Last_Preset_ID_Range
                        : constant Video.Lib.Preset_Range_Type :=
                           Last_Preset_ID.Get_ID;
      Number_Configurations
                        : constant Configuration_ID_Type :=
                           State.Get_Number_Configurations;
      Path              : constant String :=
                           (if Current_Directory'length > 0 then
                              Current_Directory & "/"
                           else
                              "") &
                           Name;
   begin
      Log_In (Debug, Quote ("file name", Name) &
         Quote (" Current_Directory", Current_Directory) &
         Quote (" path", Path) &
         " Last_Preset_ID" & Last_Preset_ID_Range'img &
         " number configurations" & Number_Configurations'img);

      Global_Camera_Setup := Setup'unchecked_access;
      Config.Load (Path, False);

      -- allocate array for configurations
      Setup.Configurations := new Configurations_Type (1 ..
         Number_Configurations);

      Setup.Path.Construct (Name);
      -- allocate array for presets
      Setup.Presets := new Presets_Type (
         Video.Lib.Preset_Range_Type'first .. Last_Preset_ID_Range);

      -- lookup preset names in config
      for Preset_Number in Video.Lib.Preset_Range_Type'first ..
            Last_Preset_ID_Range loop
         declare
            Name                 : constant String :=
                                    "preset_" & Trim (Preset_Number'img);
         begin
            Log_Here (Debug, "preset" & Preset_Number'img & Quote (" name", Name));
            if Config.Has (Name) then
               declare
                  Value          : constant String := Config.Get_String (Name);
                                    -- value is row and column #
                  Iterator       : Ada_Lib.Parser.Iterator_Type :=
                                    Ada_Lib.Parser.Initialize (Value,
                                       Ignore_Multiple_Seperators    => False,
                                       Seperators                    => ",",
                                       Trim_Spaces                   => False);
                  Row            : constant Row_Type :=
                                    Row_Type (Iterator.Get_Number (
                                       Do_Next => True));
                  Column         : constant Column_Type :=
                                    Column_Type (Iterator.Get_Number(
                                       Do_Next => True));

               begin
                  Log_Here (Debug, "preset" & Preset_Number'img &
                     Quote (" value", Value) &
                     " row" & Row'img & " column" & Column'img);

                  if Column > State.Get_Number_Columns then
                     raise Failed with "invalid column" & Column'img &
                        " maximum column" & State.Get_Number_Columns'img;
                  end if;
                  if Row > State.Get_Number_Rows then
                     raise Failed with "invalid row" & Row'img &
                        " maximum row" & State.Get_Number_Rows'img;
                  end if;

                  -- put preset in to presets array
                  Setup.Presets (Preset_Number) := Preset_Type'(
                        Initial_Root_State with
                     Column      => Column,
                     Row         => Row,
                     Preset_ID   => Video.Lib.Constructor (Preset_Number));
               end;
            else  -- if preset not defined set it to the null preset definition
               Log_Here (Debug, "preset id" & Preset_Number'img);
               Setup.Presets (Preset_Number) := Null_Preset;
            end if;
         end;
         Log_Here (Debug, Setup.Presets (Preset_Number).Preset_Image);
      end loop;

      -- look up image names to get file name
      for Configuration_ID in 1 .. Number_Configurations loop
         declare
            Name                 : constant String := "configuration_" &
                                    Trim (Configuration_ID'img);
         begin
            Log_Here (Debug, "configuration" & Configuration_ID'img
               & Quote (" name", Name));

            if Config.Has (Name) then
               declare
                  Value          : constant String := Config.Get_String (Name);
                  Iterator       : Ada_Lib.Parser.Iterator_Type :=
                                    Ada_Lib.Parser.Initialize (Value,
                                       Ignore_Multiple_Seperators    => False,
                                       Seperators                    => ",",
                                       Trim_Spaces                   => False);
                  Preset_ID      : constant Standard.Camera.Preset_ID_Type :=
                                    (if Iterator.At_End then
                                       Video.Lib.Null_Preset_ID
                                    else
                                       Video.Lib.Constructor (
                                          Video.Lib.Preset_Range_Type (
                                             Iterator.Get_Number (
                                                Do_Next => True))));
                  Label          : constant String := (if Iterator.At_End then
                                       ""
                                    else
                                       Iterator.Get_Value (Allow_Null => True));
               begin
                  Log_Here (Debug, "configuration name " & Name &
                     " value " & Configuration_ID'img & " preset id " &
                     Preset_ID.Image &
                     Quote (" label", Label));

                  if    Preset_ID.Is_Set then
                     Log_Here (Debug, "Setup.Presets" & Preset_Id.Get_ID'img &
                        " " & Setup.Presets (Preset_Id.Get_ID).Preset_ID.Image);

                     if not Setup.Presets (Preset_Id.Get_ID).Preset_ID.Is_Set then
                        raise Failed with "preset" & Preset_Id.Get_ID'img &
                           " not configured for configuration" &
                           Configuration_ID'img & " at " & Here;
                     end if;
                  end if;
                  Setup.Configurations (Configuration_ID) := (
                     Initial_Root_State with
                     Configuration_ID  => Configuration_ID,
                     Label             => Ada_Lib.Strings.Unlimited.Coerce (Label),
                     Preset_ID         => Preset_ID);
               end;
            end if;
         end;
      end loop;

      Config.Close;
      Setup.Loaded := True;
      Log_Out (Debug);

   exception

      when Fault: Failed =>
         Trace_Message_Exception (Debug, Fault, Quote ("with setup path", Path));
         Put_Line (Quote ("Load configuration setup", Path) &
            " failed with " & Ada.Exceptions.Exception_Message (Fault));
         Log_Exception (Debug);
         raise;

      when Fault: others =>
         Trace_Message_Exception (Debug, Fault, Quote ("with setup path", Path));
         Log_Exception (Debug);
         raise;

   end Load;

   ----------------------------------------------------------------
   function Make_Image_Name (
      Row                           : in     Row_Type;
      Column                        : in     Column_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return "image_" & Trim (Row'img) & "-" &Trim (Column'img);
   end Make_Image_Name;

   ----------------------------------------------------------------
   function Make_Image_Name (
      Preset                     : in     Preset_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return Make_Image_Name (Preset.Row, Preset.Column);
   end Make_Image_Name;

   ----------------------------------------------------------------
   function Preset_Column (
      Setup                      : in     Setup_Type;
      Preset                     : in     Standard.Camera.Preset_ID_Type
   ) return Column_Type is
   ----------------------------------------------------------------

   begin
      if Setup.Presets (Preset.Get_ID).Row = Row_Not_Set then
         raise Failed with "preset" & Preset'img & " not defined";
      else
         return Setup.Presets (Preset.Get_ID).Column;
      end if;
   end Preset_Column;

   ----------------------------------------------------------------
   function Preset_Image (
      Preset                     : in     Preset_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return (if Preset.Row = Row_Not_Set then
            "not set"
         else
            Trim (Preset.Column'img) &
               "," & Trim (Preset.Row'img));
   end Preset_Image;

   ----------------------------------------------------------------
   function Preset_Row (
      Setup                      : in     Setup_Type;
      Preset                     : in     Standard.Camera.Preset_ID_Type
   ) return Row_Type is
   ----------------------------------------------------------------

   begin
      if Setup.Presets (Preset.Get_ID).Row = Row_Not_Set then
         raise Failed with "preset" & Preset'img & " not defined";
      else
         return Setup.Presets (Preset.Get_ID).Row;
      end if;
   end Preset_Row;

   ----------------------------------------------------------------
   procedure Set_Path (
      Setup                      : in out Setup_Type;
      Path                       : in     String) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, Quote ("path", Path));
      Setup.Path.Construct (Path);
   end Set_Path;

   ----------------------------------------------------------------
   procedure Unload (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Save_Changes               : in     Boolean) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, Setup.Loaded'img & " Save_Changes " & Save_Changes'img &
         " modified " & Setup.Modified'img);

      if Setup.Is_Loaded then
         if Save_Changes and then Setup.Modified then
            Setup.Update (State);
            Setup.Modified := False;
         end if;
         Setup.Loaded := False;
         Free (Setup.Configurations);
         Free (Setup.Presets);
      end if;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Log_Exception (Debug);
         raise;

   end Unload;

   ----------------------------------------------------------------
   procedure Update  (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.
                                             State.State_Type'class) is
   ----------------------------------------------------------------

      Config                     : Ada_Lib.Configuration.Configuration_Type;
      Configurations             : Configurations_Access renames
                                    Setup.Configurations;
      Current_Directory          : constant String :=
                                    Standard.Camera.Lib.Options.Current_Directory;
      Presets                    : Presets_Access renames
                                    Setup.Presets;
   begin
      Log_In (Debug,
         Quote ("Current_Directory", Current_Directory) &
         Quote (" path", Setup.Path));

      for Preset_Number in Video.Lib.Preset_Range_Type'first ..
            Video.Lib.Get_Last_Preset_ID.Get_ID loop
         declare
            Name                 : constant String :=
                                    "preset_" & Trim (Preset_Number'img);
            Preset               : Preset_Type renames Presets (Preset_Number);

         begin
            Log_Here (Debug, "preset" & Preset_Number'img & Quote (" name", Name));
            if Preset.Row /= Row_Not_Set then
               Config.Set (Name, Trim (Preset.Row'img) & "," &
                  Trim (Preset.Column'img), Update => False);
            end if;
         end;
      end loop;

      for Configuration_ID in 1 .. State.Get_Number_Configurations loop
         declare
            Name                 : constant String := "configuration_" &
                                    Trim (Configuration_ID'img);
            Configuration        : Configuration_Type renames
                                    Configurations (Configuration_ID);
         begin
            Log_Here (Debug, "configuration" & Configuration_ID'img
               & Quote (" name", Name) &
               " preset id" & Configuration.Preset_Id.Get_ID'img &
               Quote (" label", Configuration.Label));

            if Configuration.Preset_ID.Is_Set then
               Config.Set (Name, Trim (Configuration.Preset_Id.Get_ID'img) & "," &
                  Configuration.Label.Coerce, Update => False);
            end if;
         end;
      end loop;

      Config.Store (Setup.Path.Coerce);
      Log_Out (Debug);

   exception

      when Fault: Failed =>
         Trace_Message_Exception (Debug, Fault, Quote ("with setup path",
            Setup.Path));
         Put_Line (Quote ("Load configuration setup", Setup.Path.Coerce) &
            " failed with " & Ada.Exceptions.Exception_Message (Fault));
         Log_Exception (Debug);
         raise;

      when Fault: others =>
         Trace_Message_Exception (Debug, Fault, Quote ("with setup path",
            Setup.Path));
         Log_Exception (Debug);
         raise;

   end Update;

   ----------------------------------------------------------------
   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Preset_ID                  : in     Standard.Camera.Preset_ID_Type) is
   ----------------------------------------------------------------

      Configuration              : Configuration_Type renames
                                    Setup.Configurations (
                                       Configuration_ID);
   begin
      Log_Here (Debug, "Configuration" & Configuration_ID'img &
         " Preset_ID" & Preset_ID.Image);

      Configuration.Preset_ID := Preset_ID;
      Setup.Modified := True;
   end Update_Configuration;

   ----------------------------------------------------------------
   procedure Update_Configuration (
      Setup                      : in out Setup_Type;
      Configuration_ID           : in     Configuration_ID_Type;
      Label                      : in     String) is
   ----------------------------------------------------------------

      Configuration              : Configuration_Type renames
                                    Setup.Configurations (
                                       Configuration_ID);
   begin
      Log_Here (Debug, "Configuration" & Configuration_ID'img &
         Quote (" Label", Label));

      Configuration.Label.Construct (Label);
      Setup.Modified := True;
   end Update_Configuration;

   ----------------------------------------------------------------
   procedure Update_Preset (
      Setup                      : in out Setup_Type;
      Preset_ID                  : in     Standard.Camera.Preset_ID_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type) is
   ----------------------------------------------------------------

      Preset                     : Preset_Type renames
                                    Setup.Presets (Preset_Id.Get_ID);
   begin
      Log_Here (Debug, "preset id" & Preset_ID.Image &
         " row" & Row'img & " column" & Column'img);
      Preset.Column := Column;
      Preset.Row := Row;
      Setup.Modified := True;
   end Update_Preset;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Elaborate);

end Configuration.Camera.Setup;
