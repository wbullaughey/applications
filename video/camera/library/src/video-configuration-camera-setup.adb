with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with Ada_Lib.Address;
with Ada_Lib.Configuration;
--with Ada_Lib.Options;
with Ada_Lib.Parser;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with ADA_LIB.Strings;use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Base;
with Camera.Lib.Options;

package body Video.Configuration.Camera.Setup is

   use type Standard.Camera.Preset_ID_Type;
-- use type Video.Preset.Preset_ID_Type;

   procedure Free is new Ada.Unchecked_Deallocation (
      Configurations_Type,
      Configurations_Access);

   procedure Free is new Ada.Unchecked_Deallocation (
      Preset.Presets_Type,
      Preset.Presets_Access);

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
      Preset.Dump_Presets;
      Put_Line ("Loaded " & Setup.Is_Loaded'img);
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
      Put_Line ("  Preset ID:" & Configuration.Preset_ID'img);
   end Dump;

   ----------------------------------------------------------------
   function Find_Configuration (
      Preset               : in     Video.Preset.Preset_Type
   ) return Configuration_Constant_Access is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "preset " & Preset.Image);

      for Configuration of Global_Camera_Setup.Configurations.all loop
         Log_Here (Debug, Configuration.Image);
         if Configuration.Preset_ID = Preset.Get_Preset_ID then
            Log_Out (Debug, Configuration.Image);
            return Configuration'unchecked_access;
         end if;
      end loop;

      Log_Out (Debug, Preset.Image);
      raise Failed with "Preset " & Preset.Image & " not found" &
         " at " & Here;
      return Null;
   end find_Configuration;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Configuration        : in     Configuration_Type
   ) return Standard.Camera.Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      return Configuration.Preset_ID;
   end Get_Preset_ID;

   ----------------------------------------------------------------
   function Get_Preset_ID (
      Preset               : in     Video.Preset.Preset_Type
   ) return Standard.Camera.Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      return Preset.Get_Preset_ID;
   end Get_Preset_ID;

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
   procedure Free (
      Configurations          : in     Configurations_Type) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
   end Free;

   ----------------------------------------------------------------
   procedure Free (
      Setup                   : in     Setup_Type) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      if Setup.Configurations /= Null then
         Free (Setup.Configurations.all);
      end if;
   end Free;

   ----------------------------------------------------------------
   function Get_Configuration (
      Setup                      : in     Setup_Type;
      Configuration_ID        : in     Configuration_ID_Type
   ) return Configuration_Type'class is
   ----------------------------------------------------------------

   begin
      return Setup.Configurations (Configuration_ID);
   end Get_Configuration;

-- ----------------------------------------------------------------
-- function Get_Preset (
--    Setup                      : in     Setup_Type;
--    Preset_Id                  : in     Standard.Camera.Preset_ID_Type
-- ) return Preset.Preset_Type'class is
-- ----------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "Preset_Id " & Preset_Id.Get_ID'img);
--    return (if Preset_ID /= Preset.Preset_ID_Not_Set then
--          Setup.Presets (Preset_Id.Get_ID)
--       else
--          Null_Preset);
-- end Get_Preset;

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
         " result " & Result'img);
      return Result;
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
      Camera_ID                  : in     Lib.Camera_ID_Type;
      Preset_Id                  : in     Standard.Camera.Preset_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

      ID       : constant Video.Preset.Preset_ID_Type := Preset_Id;
      Trace    : constant String := "preset id " & Preset_ID'img & " ID" & ID'img;

   begin
      Log_In (Debug, Trace);

      if not Preset.Is_Valid (Camera_ID, ID) then
         return Log_Out (False, Debug, "ID" & ID'img &" out of range " &
            Setup.Presets'first'img & " .." & Setup.Presets'last'img);
      end if;

      declare
         Row         : constant Row_Type := Setup.Presets (ID).Row;
         Has_Row     : constant Boolean := Row /= Row_Not_Set;
         Result      : constant Boolean := Preset_ID /= Preset.Preset_ID_Not_Set and then Has_Row;

      begin
         return Log_Out (Result, Debug, "Is_Set " & Preset_ID /= Preset.Preset_ID_Not_Set'img &
            " Has_Row " & Has_Row'img &
            " ID" & ID'img & " row" & Row'img);
      end;

   exception

      when others =>
         Log_Exception (Debug, Trace & " presets range " &
            Setup.Presets'first'img & " .." & Setup.Presets'last'img);
         raise;

   end Has_Preset;

   ----------------------------------------------------------------
   function Has_Presets (
      Setup                      : in     Setup_Type;
      Expect                     : in     Boolean
   ) return Boolean is
   ----------------------------------------------------------------

      Result   : constant Boolean := Setup.Presets /= Null;

   begin
      return Log_Here (Result, Trace_Pre_Post (Debug, Expect));
   end Has_Presets;

   ----------------------------------------------------------------
   function Image (
      Configuration        : in     Configuration_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return "configuration id" & Configuration.Configuration_ID'img &
         Quote (" label", Configuration.Label) &
         " preset " & Configuration.Preset_ID'img;
   end Image;

   ----------------------------------------------------------------
   function Image (
      Preset                     : in     Preset.Preset_Type;
      Label                      : in     Boolean := True
   ) return String is
   ----------------------------------------------------------------

   begin
      return (if Preset.Get_Row = Row_Not_Set then
            "not set"
         else (if Label then
                  "column" & Preset.Get_Column'img & " " &
                  Preset.Get_Preset_ID'img &
                  " row" & Preset.Get_Row'img
               else
                  Preset.Get_Preset_ID'img &
                  Preset.Get_Column'img &
                  "," & Trim (Preset.Get_Row'img)));

   end Image;

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

      Configuration  : Ada_Lib.Configuration.Configuration_Type;

   begin
      Log_In (Debug, Quote ("file name", Name));

      declare
--       Config            : Ada_Lib.Configuration.Configuration_Type;
         Current_Directory : constant String :=
                              Standard.Camera.Lib.Options.Current_Directory;
         Last_Preset_ID    : constant Standard.Camera.Preset_ID_Type :=
                              Preset.Get_Last_Preset_ID;
         Last_Preset_ID_Range
                           : constant Video.Preset.Preset_ID_Type :=
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
         Log_Here (Debug, Quote ("file name", Name) &
            Quote (" Current_Directory", Current_Directory) &
            Quote (" path", Path) &
            " Last_Preset_ID" & Last_Preset_ID_Range'img &
            " number configurations" & Number_Configurations'img);

         Global_Camera_Setup := Setup'unchecked_access;
         Configuration.Load (Path, False);

         -- allocate array for configurations
         Setup.Configurations := new Configurations_Type (1 ..
            Number_Configurations);

         Setup.Path.Construct (Name);
         -- allocate array for presets
         Setup.Presets := new Presets_Type (
            Video.Preset.Preset_ID_Type'first .. Last_Preset_ID_Range);

         -- lookup preset names in Configuration
         for Preset_Number in Video.Preset.Preset_ID_Type'first ..
               Last_Preset_ID_Range loop
            declare
               Name     : constant String :=
                           "preset_" & Trim (Preset_Number'img);
               Preset   : Preset.Preset_Type renames Setup.Presets (Preset_Number);

            begin
               Log_Here (Debug, "preset" & Preset_Number'img & Quote (" name", Name));
               if Configuration.Has (Name) then
                  declare
                     Value          : constant String := Configuration.Get_String (Name);
                                       -- value is row and column #
                     Iterator       : Ada_Lib.Parser.Iterator_Type :=
                                       Ada_Lib.Parser.Initialize (Value,
                                          Ignore_Multiple_Seperators    => False,
                                          Seperators                    => ",",
                                          Trim_Spaces                   => False);
                     Camera_ID      : constant Camera_ID_Type :=
                                       Camera_ID_Type (Iterator.Get_Number (
                                          Do_Next => True));
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
                     Preset := Video.Preset.Constructor (
                        Camera_ID   => Camera_ID,
                        Column      => Column,
                        Row         => Row,
                        Preset_ID   => Preset.Constructor (Preset_Number));
                  end;
               else  -- if preset not defined set it to the null preset definition
                  Log_Here (Debug, "preset id" & Preset_Number'img);
                  Setup.Presets (Preset_Number) := Null_Preset;
               end if;
               Preset.Set_Loaded (True);
            end;
            Log_Here (Debug, Setup.Presets (Preset_Number).Image);
         end loop;

         -- look up image names to get file name
         for Configuration_ID in 1 .. Number_Configurations loop
            declare
               Name                 : constant String := "configuration_" &
                                       Trim (Configuration_ID'img);
            begin
               Log_Here (Debug, "configuration" & Configuration_ID'img
                  & Quote (" name", Name));

               if Configuration.Has (Name) then
                  declare
                     Value          : constant String := Configuration.Get_String (Name);
                     Iterator       : Ada_Lib.Parser.Iterator_Type :=
                                       Ada_Lib.Parser.Initialize (Value,
                                          Ignore_Multiple_Seperators    => False,
                                          Seperators                    => ",",
                                          Trim_Spaces                   => False);
                     Preset_ID      : constant Standard.Camera.Preset_ID_Type :=
                                       (if Iterator.At_End then
                                          Video.Preset.Preset_ID_Not_Set
                                       else
                                          Video.Preset.Preset_ID_Type (
                                             Iterator.Get_Number (
                                                Do_Next => True)));
                     Label          : constant String := (if Iterator.At_End then
                                          ""
                                       else
                                          Iterator.Get_Value (Allow_Null => True));
                  begin
                     Log_Here (Debug, "configuration name " & Name &
                        " value " & Configuration_ID'img & " preset id " &
                        Preset_ID'img &
                        Quote (" label", Label));

--                   if    Preset_ID /= Preset.Preset_ID_Not_Set then
--                      Log_Here (Debug, "Setup.Presets" & Preset_Id.Get_ID'img &
--                         " " & Setup.Presets (Preset_Id.Get_ID).Preset_ID'img);
--
--                      if not Setup.Presets (Preset_Id.Get_ID).Preset_ID /= Preset.Preset_ID_Not_Set then
--                         raise Failed with "preset" & Preset_Id.Get_ID'img &
--                            " not configured for configuration" &
--                            Configuration_ID'img & " at " & Here;
--                      end if;
--                   end if;
                     Setup.Configurations (Configuration_ID) := (
                        Configuration_Package.Configuration_Type with
                        Configuration_ID  => Configuration_ID,
                        Label             => Ada_Lib.Strings.Unlimited.Coerce (Label),
                        Preset_ID         => Preset_ID);
                  end;
               end if;
            end;
         end loop;

         Configuration.Close;
         Setup.Set_Loaded (True);
      exception

         when Fault: Failed =>
            Trace_Message_Exception (Debug, Fault, Quote ("with setup path", Path));
            Put_Line (Quote ("Load configuration setup", Path) &
               " failed with " & Ada.Exceptions.Exception_Message (Fault));
            Log_Exception (Debug, Fault);
            raise;

         when Fault: others =>
            Trace_Message_Exception (Debug, Fault, Quote ("with setup path", Path));
            Log_Exception (Debug, Fault);
            raise;

      end;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault);
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
      Log_In (Debug, Setup.Is_Loaded'img &
         " Save_Changes " & Save_Changes'img &
         " modified " & Setup.Modified'img);

      if Setup.Is_Loaded then
         if Save_Changes and then Setup.Modified then
            Setup.Update (State);
            Setup.Modified := False;
         end if;
         Setup.Set_Loaded (False);
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
         Quote (" path", Setup.Path) &
         " class " & Ada.Tags.Expanded_Name (
            Setup_Type'class (Setup)'tag));

Tag_History ("Setup", Setup_Type'class (Setup)'tag);

--      -- loop over all presets
--      for Preset_Number in Video.Preset.Preset_ID_Type'first ..
--            Preset.Get_Last_Preset_ID.Get_ID loop
--         declare
--            Name                 : constant String :=
--                                    "preset_" & Trim (Preset_Number'img);
--            Preset               : Preset.Preset_Type renames Presets (Preset_Number);
--
--         begin
--            Log_Here (Debug, "preset" & Preset_Number'img &
--               Quote (" name", Name) &
--               " row" & Preset.Get_Row'img);
--            if Preset.Get_Row /= Row_Not_Set then
--               declare
--                  Configuration  : Video.Configuration.Camera.
--                                    Setup.Configuration_Type renames
--                                       Preset.Find_Configuration.all;
--               begin
--                  Configuration.Set (Name, Trim (Preset.Get_Row'img) & "," &
--                     Trim (Preset.Get_Column'img), Update => False);
--               end;
--            end if;
--         end;
--      end loop;
--log_here;

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

--          if Configuration.Preset_ID /= Preset.Preset_ID_Not_Set then
--             Setup.Set (Name, Trim (Configuration.Preset_Id.Get_ID'img) & "," &
--                Configuration.Label.Coerce, Update => False);
--          else
--          end if;
         end;
      end loop;

--    Setup.Store (Setup.Path.Coerce);
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
      Preset_ID                  : in     Standard.Camera.Preset_ID_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type) is
   ----------------------------------------------------------------

      Preset                     : Preset.Preset_Type renames
                                    Setup.Presets (Preset_Id.Get_ID);
   begin
      Log_Here (Debug, "preset " & Preset_ID'img &
         " row" & Row'img & " column" & Column'img);
      Preset.Get_Column := Column;
      Preset.Row := Row;
      Setup.Modified := True;
   end Update_Preset;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Elaborate);

end Video.Configuration.Camera.Setup;
