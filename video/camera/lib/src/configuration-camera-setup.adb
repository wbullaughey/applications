with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Address;
with Ada_Lib.Configuration;
with Ada_Lib.Options;
with Ada_Lib.Parser;
with ADA_LIB.Strings.Unlimited; use Ada_Lib.Strings; use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Options;

package body Configuration.Camera.Setup is

-- use type System.Address;

-- package Configuration_Address_Converter is new
--    System.Address_To_Access_Conversions (Configuration_Type);

   procedure Free is new Ada.Unchecked_Deallocation (
      Configurations_Type,
      Configurations_Access);

   procedure Free is new Ada.Unchecked_Deallocation (
      Presets_Type,
      Presets_Access);

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
   ) return Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      return Setup.Configurations (Configuration_ID).Preset_ID;
   end Configuration_Preset;

   ----------------------------------------------------------------
   procedure Dump (
      Setup                      : in     Setup_Type;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_LIne ("dump setup from " & From);
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
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("Configuration" & Configuration.Configuration_ID'img &
         " from " & From);
      Put_Line (Quote ("  label", Configuration.Label));
      Put_Line ("  Preset ID:" & Configuration.Preset_ID'img);
   end Dump;

   ----------------------------------------------------------------
   procedure Dump (
      Preset                     : in     Preset_Type;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("Preset" & Preset.Preset_ID'img & " from " & From);
      Put_Line ("  Column:" & Preset.Column'img);
      Put_Line ("  Row:" & Preset.Row'img);
   end Dump;

   ----------------------------------------------------------------
   function File_Path
   return String is
   ----------------------------------------------------------------

      Camera_Options : Standard.Camera.Lib.Options.Camera_Options_Type'class renames
                        Standard.Camera.Lib.Options.Get_Read_Only_Camera_Options.all;
   begin
      return (if Camera_Options.Setup_Path.Length > 0 then
         Camera_Options.Setup_Path.Coerce
      else
         Default_Setup);
   end File_Path;

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
      Preset_Id                  : in     Preset_ID_Type
   ) return Preset_Type'class is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "Preset_Id " & Preset_Id'img);
      return (if Preset_ID = Preset_Not_Set then
            Null_Preset
         else
            Setup.Presets (Preset_Id));
   end Get_Preset;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Configuration_Id           : in     Configuration_ID_Type
   ) return Preset_ID_Type is
   ----------------------------------------------------------------

      Result                     : constant Preset_ID_Type :=
                                    Setup.Configurations (
                                       Configuration_Id).Preset_ID;
   begin
      Log_Here (Debug, "Configuration_Id " & Configuration_Id'img &
         " result " & Result'img);
      return Result;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Setup                      : in     Setup_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "row" & Row'img & " column" & Column'img);
      for ID in Setup.Presets'first .. Setup.Presets.all'last loop
         declare
            Preset               : Preset_Type renames Setup.Presets (ID);

         begin
            if Preset.Row = Row and then Preset.Column = Column then
               Log_Out (Debug, "preset" & Id'img);
               return ID;
            end if;
         end;
      end loop;

      Log_Out (Debug, "not defined");
      return Preset_Not_Set;
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
      Preset_Id                  : in     Preset_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img);

      declare
         Result                     : constant Boolean :=
                                       Preset_ID /= Preset_Not_Set and then
                                       Setup.Presets (
                                          Preset_Id).Row /= Row_Not_Set;
      begin
         return Log_Out (Result, Debug, "preset id" & Preset_ID'img);
      end;
   end Has_Preset;

   ----------------------------------------------------------------
   function Is_Loaded (
      Setup                      : in     Setup_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Setup.Loaded, Debug,
         "loaded " & Setup.Loaded'img);
   end Is_Loaded;

   ----------------------------------------------------------------
   procedure Load (
      Setup                      : in out Setup_Type;
      State                      : in     Configuration.Camera.State.State_Type'class;
      Name                       : in     String) is
   ----------------------------------------------------------------

begin
log_here;
declare
      Config                     : Ada_Lib.Configuration.Configuration_Type;
      Current_Directory          : constant String :=
                                    Standard.Camera.Lib.Options.Current_Directory;
      Path                       : constant String :=
                                    (if Current_Directory'length > 0 then
                                       Current_Directory & "/"
                                    else
                                       "") &
                                    Name;
   begin
      Log_In (Debug, Quote ("file name", Name) &
         Quote (" Current_Directory", Current_Directory) &
         Quote (" path", Path));

      Global_Camera_Setup := Setup'unchecked_access;
      Config.Load (Path, False);

      Setup.Configurations := new Configurations_Type (1 ..
         State.Number_Configurations);
      Setup.Path.Construct (Name);
      Setup.Presets := new Presets_Type (Preset_ID_Type'first ..
         State.Last_Preset);

      for Preset_ID in Setup.Presets'first .. State.Last_Preset loop
         declare
            Name                 : constant String :=
                                    "preset_" & Trim (Preset_ID'img);
         begin
            Log_Here (Debug, "preset" & Preset_ID'img & Quote (" name", Name));
            if Config.Has (Name) then
               declare
                  Value          : constant String := Config.Get_String (Name);
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
                  Log_Here (Debug, Quote ("value", Value) &
                     " row" & Row'img & " column" & Column'img);

                  if Column > State.Number_Columns then
                     raise Failed with "invalid column" & Column'img &
                        " maximum column" & State.Number_Columns'img;
                  end if;
                  if Row > State.Number_Rows then
                     raise Failed with "invalid row" & Row'img &
                        " maximum row" & State.Number_Rows'img;
                  end if;

                  Setup.Presets (Preset_ID) := Preset_Type'(
                     Column      => Column,
                     Row         => Row,
                     Preset_ID   => Preset_ID,
                     Loaded      => True,
                     Updated     => False);
               end;
            else
               Log_Here (Debug, "preset id" & Preset_ID'img);
               Setup.Presets (Preset_ID) := Null_Preset;
            end if;
         end;
         Log_Here (Debug, Setup.Presets (Preset_ID).Preset_Image);
      end loop;

      for Configuration_ID in 1 .. State.Number_Configurations loop
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
                  Preset_ID      : constant Preset_ID_Type :=
                                    (if Iterator.At_End then
                                       Preset_Not_Set
                                    else
                                       Preset_ID_Type (Iterator.Get_Number (
                                          Do_Next => True)));
                  Label          : constant String := (if Iterator.At_End then
                                       ""
                                    else
                                       Iterator.Get_Value (Allow_Null => True));
                  Name           : constant String :=
                                    "configuration_" & Trim (Configuration_ID'img);
               begin
                  Log_Here (Debug, "configuration name " & Name &
                     " value " & Configuration_ID'img & " preset id" & Preset_ID'img &
                     Quote (" label", Label));

                  if    Preset_ID /= Preset_Not_Set and then
                        Setup.Presets (Preset_ID) = Null_Preset then
                     raise Failed with "preset" & Preset_ID'img &
                        " not configured for configuration" & Configuration_ID'img;
                  end if;
                  Setup.Configurations (Configuration_ID) := (
                     Configuration_ID  => Configuration_ID,
                     Label             => Ada_Lib.Strings.Unlimited.Coerce (Label),
                     Preset_ID         => Preset_ID,
                     Loaded            => True,
                     Updated           => False);
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

end;
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
      Preset                     : in     Preset_ID_Type
   ) return Column_Type is
   ----------------------------------------------------------------

   begin
      if Setup.Presets (Preset).Row = Row_Not_Set then
         raise Failed with "preset" & Preset'img & " not defined";
      else
         return Setup.Presets (Preset).Column;
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
      Preset                     : in     Preset_ID_Type
   ) return Row_Type is
   ----------------------------------------------------------------

   begin
      if Setup.Presets (Preset).Row = Row_Not_Set then
         raise Failed with "preset" & Preset'img & " not defined";
      else
         return Setup.Presets (Preset).Row;
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

      for Preset_ID in Setup.Presets'first .. State.Last_Preset loop
         declare
            Name                 : constant String :=
                                    "preset_" & Trim (Preset_ID'img);
            Preset               : Preset_Type renames Presets (Preset_ID);

         begin
            Log_Here (Debug, "preset" & Preset_ID'img & Quote (" name", Name));
            if Preset.Row /= Row_Not_Set then
               Config.Set (Name, Trim (Preset.Row'img) & "," &
                  Trim (Preset.Column'img), Update => False);
            end if;
         end;
      end loop;

      for Configuration_ID in 1 .. State.Number_Configurations loop
         declare
            Name                 : constant String := "configuration_" &
                                    Trim (Configuration_ID'img);
            Configuration        : Configuration_Type renames
                                    Configurations (Configuration_ID);
         begin
            Log_Here (Debug, "configuration" & Configuration_ID'img
               & Quote (" name", Name) &
               " preset id" & Configuration.Preset_ID'img &
               Quote (" label", Configuration.Label));

            if Configuration.Preset_ID /= Preset_Not_Set then
               Config.Set (Name, Trim (Configuration.Preset_ID'img) & "," &
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
      Preset_ID                  : in     Preset_ID_Type) is
   ----------------------------------------------------------------

      Configuration              : Configuration_Type renames
                                    Setup.Configurations (
                                       Configuration_ID);
   begin
      Log_Here (Debug, "Configuration" & Configuration_ID'img &
         " Preset_ID" & Preset_ID'img);

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
      Preset_ID                  : in     Preset_ID_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type) is
   ----------------------------------------------------------------

      Preset                     : Preset_Type renames
                                    Setup.Presets (Preset_ID);
   begin
      Log_Here (Debug, "preset id" & Preset_ID'img &
         " row" & Row'img & " column" & Column'img);
      Preset.Column := Column;
      Preset.Row := Row;
      Setup.Modified := True;
   end Update_Preset;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);

end Configuration.Camera.Setup;
