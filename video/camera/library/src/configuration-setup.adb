with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Unchecked_Deallocation;
--with Ada_Lib.Address;
with Ada_Lib.Configuration;
--with Ada_Lib.Options;
with Ada_Lib.Parser;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with ADA_LIB.Strings;use Ada_Lib.Strings;
with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Options;
with Camera.Preset;
--with Configuration.Configuration;
with Configuration.State;
--with Video.Lib;

package body Configuration.Setup is

-- use type Video.Lib.Preset_ID_Type;

   Configuration_Path: Ada_Lib.Strings.Unlimited.String_Type;
   Debug             : Boolean renames Standard.Camera.Lib.Options.
                        Configuration_Options.Setup_Debug;
   Loaded            : Boolean := False;
   Modified          : Boolean := False;
   Number_Cameras    : Video.Lib.Camera_ID_Type := Video.Lib.Camera_ID_Not_Set;
   Number_Configurations
                     : Configuration.Configuration_ID_Type :=
                        Configuration.Configuration_Not_Set;
   Number_Presets    : Video.Lib.Preset_ID_Type := Video.Lib.Preset_ID_Not_Set;

-- ----------------------------------------------------------------
-- function Setup_Label (
--    Setup_ID           : in     Setup_ID_Type
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return Setup.Label.Coerce;
-- end Setup_Label;

-- ----------------------------------------------------------------
-- function Configuration_Preset (
--    Configuration_ID           : in     Configuration.Configuration_ID_Type
-- ) return Video.Lib.Preset_ID_Type is
-- ----------------------------------------------------------------
--
--    Result   : constant Video.Lib.Preset_ID_Type :=
--                Configurations (Configuration_ID).Preset_ID;
-- begin
--    Log_Here (Debug, "Configuration_ID" & Configuration_ID'img &
--       " preset id" & Result'img);
--    return Result;
-- end Configuration_Preset;

-- ----------------------------------------------------------------
-- procedure Dump (
--    What                       : in     String := "";
--    From                       : in     String := Ada_Lib.Trace.Here) is
-- ----------------------------------------------------------------
--
-- begin
--    Put_LIne ("dump " & (if What'length > 0 then
--          "what " & What & " "
--       else
--          "") &
--       "setup from " & From);
--    for Configuration in Configurations.all'range loop
--       Put_Line ("Configuration" & Configuration'img);
--       Configurations (Configuration).Dump;
--    end loop;
--    for Preset in Setup.Presets.all'range loop
--       Put_Line ("Preset" & Preset'img);
--       Setup.Presets (Preset).Dump;
--    end loop;
--    Put_Line ("Loaded " & Setup.Is_Loaded'img);
--    Put_Line ("Modified " & Setup.Modified'img);
-- end Dump;

   ----------------------------------------------------------------
   procedure Dump (
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("Setup called from " & From);
      Put_Line ("number cameras" & Number_Cameras'img);
      Put_Line ("number presets" & Number_Presets'img);
--    Put_Line ("number configurations" & Number_Configurations'img);
   end Dump;

-- ----------------------------------------------------------------
-- procedure Dump (
--    Preset                     : in     Preset_Type;
--    What                       : in     String := "";
--    From                       : in     String := Ada_Lib.Trace.Here) is
-- ----------------------------------------------------------------
--
-- begin
--    Put_Line ((if What'length > 0 then
--          "what " & What & " "
--       else
--          "") &
--       Preset.Preset_ID.Image & " from " & From);
--    if Preset.Preset_ID.Is_Set then
--       Put_Line ("  Column:" & Preset.Column'img);
--       Put_Line ("  Row:" & Preset.Row'img);
--    end if;
-- end Dump;

-- ----------------------------------------------------------------
-- function Get_Column (
--    Preset               : in     Preset_Type
-- ) return Column_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Preset.Column;
-- end Get_Column;
--
   ----------------------------------------------------------------
   function Get_Number_Presets
   return Video.Lib.Preset_ID_Type is
   ----------------------------------------------------------------

   begin
      return Number_Presets;
   end Get_Number_Presets;

-- ----------------------------------------------------------------
-- function Get_Preset_ID (
--    Configuration        : in     Configuration_Type
-- ) return Video.Lib.Preset_ID_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Configuration.Preset_ID;
-- end Get_Preset_ID;
--
-- ----------------------------------------------------------------
-- function Get_Preset_ID (
--    Preset               : in     Preset_Type
-- ) return Video.Lib.Preset_ID_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Preset.Preset_ID;
-- end Get_Preset_ID;
--
-- ----------------------------------------------------------------
-- function Get_Row (
--    Preset               : in     Preset_Type
-- ) return Row_Type is
-- ----------------------------------------------------------------
--
-- begin
--    return Preset.Row;
-- end Get_Row;

-- ----------------------------------------------------------------
-- function File_Path
-- return String is
-- ----------------------------------------------------------------
--
--    Options     : constant Standard.Camera.Lib.Options.
--                   Program_Options_Constant_Class_Access :=
--                      Standard.Camera.Lib.Options.
--                   Program_Options_Constant_Class_Access (
--                      Ada_Lib.Options.Verification.Get_Ada_Lib_Constant_Nested_Options);
--    Setup_Path  : Ada_Lib.Strings.Unlimited.String_Type
--                renames Options.Nested_Options.Setup_Path;
-- begin
--    return (if Setup_Path.Length > 0 then
--                Setup_Path.Coerce
--            else
--                Default_Setup);
-- end File_Path;

--   ----------------------------------------------------------------
--   procedure Free (
--      Setups          : in     Setups_Type) is
--   ----------------------------------------------------------------
--
--   begin
--      Log_Here (Debug);
--not_implemented;
--   end Free;

-- ----------------------------------------------------------------
-- procedure Free (
--    Setup                   : in     Setup_Type) is
-- ----------------------------------------------------------------
--
-- begin
--    Log_Here (Debug);
--    if Configurations /= Null then
--       Free (Configurations.all);
--    end if;
-- end Free;

-- ----------------------------------------------------------------
-- function Get_Configuration (
--    Configuration_ID        : in     Configuration.Configuration_ID_Type
-- ) return Configuration_Type'class is
-- ----------------------------------------------------------------
--
-- begin
--    return Configurations (Configuration_ID);
-- end Get_Configuration;
--
-- ----------------------------------------------------------------
-- function Get_Preset (
--    Preset_Id                  : in     Video.Lib.Preset_ID_Type
-- ) return Preset_Type'class is
-- ----------------------------------------------------------------
--
-- begin
--    Log_Here (Debug, "Preset_Id " & Preset_Id.Get_ID'img);
--    return (if Preset_ID.Is_Set then
--          Setup.Presets (Preset_Id.Get_ID)
--       else
--          Null_Preset);
-- end Get_Preset;
--
-- ----------------------------------------------------------------
-- function Get_Preset_ID (
--    Configuration_Id           : in     Configuration.Configuration_ID_Type
-- ) return Video.Lib.Preset_ID_Type is
-- ----------------------------------------------------------------
--
--    Result                     : constant Video.Lib.Preset_ID_Type :=
--                                  Configurations (
--                                     Configuration_Id).Preset_ID;
-- begin
--    Log_Here (Debug, "Configuration_Id " & Configuration_Id'img &
--       " result " & Result.Image);
--    return Result;
-- end Get_Preset_ID;

-- ----------------------------------------------------------------
-- function Get_Preset_ID (
--    Row                        : in     Row_Type;
--    Column                     : in     Column_Type
-- ) return Video.Lib.Preset_ID_Type is
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "row" & Row'img & " column" & Column'img);
--    for ID in Setup.Presets'first .. Setup.Presets.all'last loop
--       declare
--          Preset               : Preset_Type renames Setup.Presets (ID);
--
--       begin
--          if Preset.Row = Row and then Preset.Column = Column then
--             Log_Out (Debug, "preset" & Id'img &
--                 " row" & Row'img & " column" & Column'img);
--             return Preset.Preset_ID;
--          end if;
--       end;
--    end loop;
--
--    Log_Out (Debug, "not defined");
--    return Video.Lib.Null_Preset_ID;
-- end Get_Preset_ID;

-- ----------------------------------------------------------------
-- function Has_Setup (
--    Setup_ID                   : in     Setup_ID_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "setup id" & Setup_ID'img);
--    if    Setup_ID <= Setup.Setups.all'last then
--       declare
--          Result                     : constant Boolean :=
--                                        Setup.Setups (
--                                           Setup_ID) /=
--                                           Null_Setup;
--       begin
--          return Log_Out (Result, Debug, "setup" &
--             Setup_ID'img);
--       end;
--    else
--       Log_Exception (Debug);
--       raise Failed with "invalid setup id" & Setup_ID'img;
--    end if;
-- end Has_Setup;

-- ----------------------------------------------------------------
-- function Has_Preset (
--    Preset_Id                  : in     Video.Lib.Preset_ID_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
--    ID       : constant Video.Lib.Preset_Range_Type := Preset_Id.Get_ID;
--    Trace    : constant String := "preset id " & Preset_ID.Image & " ID" & ID'img;
--
-- begin
--    Log_In (Debug, Trace);
--
--    if ID > Setup.Presets'last then
--       return Log_Out (False, Debug, "ID" & ID'img &" out of range " &
--          Setup.Presets'first'img & " .." & Setup.Presets'last'img);
--    end if;
--
--    declare
--       Row         : constant Row_Type := Setup.Presets (ID).Row;
--       Has_Row     : constant Boolean := Row /= Row_Not_Set;
--       Result      : constant Boolean := Preset_ID.Is_Set and then Has_Row;
--
--    begin
--       return Log_Out (Result, Debug, "Is_Set " & Preset_ID.Is_Set'img &
--          " Has_Row " & Has_Row'img &
--          " ID" & ID'img & " row" & Row'img);
--    end;
--
-- exception
--
--    when others =>
--       Log_Exception (Debug, Trace & " presets range " &
--          Setup.Presets'first'img & " .." & Setup.Presets'last'img);
--       raise;
--
-- end Has_Preset;

   ----------------------------------------------------------------
   function Is_Loaded
   return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Loaded, Debug, "loaded " & Loaded'img);
   end Is_Loaded;

-- ---------------------------------------------------------------
-- procedure Load (
--    Setups       : in out Setups_Type;
--    Path                 : in     String;
--    Location             : in     Video.Lib.Location_Type) is
-- ---------------------------------------------------------------
--
--    ------------------------------------------------------------
--    function Value_Name (
--       Base              : in     String;
--       Index             : in     Positive
--    ) return String is
--    ------------------------------------------------------------
--
--    begin
--       return Base & Trim (Index'img);
--    end Value_Name;
--    ------------------------------------------------------------
--
--    Config               : Ada_Lib.Setup.Setup_Type;
--
-- begin
--    Log_In (Debug, Quote ("path", Path));
--    Config.Load (Path, False);
--
--    Setups.Number_Setups := Natural (Config.Get_Integer (
--       "number_cameras"));
--    Setups.Setups := new Setup_Array (
--                                        1 .. Setups.Number_Setups);
--
--    Log_Here (Debug, "Number_Setups" &
--       Setups.Number_Setups'img);
--
--    for Index in 1 .. Setups.Number_Setups loop
--       Log_Here (Debug, "index" & Index'img);
--       declare
--          Setup_Name     : constant String := "setup_camera-" &
--                            Trim (Index'img);
--          Setup_Path     : constant String := Config.Get_String (Setup_Name);
--          State_Name     : constant String := "state_camera-" &
--                            Trim (Index'img);
--          State_Path     : constant String := Config.Get_String (State_Name);
--          Setup  : constant Setup_Access :=
--                            new Setup_Type;
--          Setup          : constant Setup_Setup_Access :=
--                            new Setup.Camera.Setup.Setup_Type;
--          State          : constant Setup_State_Access :=
--                            new Standard.Setup.State.State_Type;
--       begin
--          Log_Here (Debug,
--             Quote ("Setup_Name", Setup_Name) &
--             Quote (" Setup_Path", Setup_Path) &
--             Quote (" State_Name", State_Name) &
--             Quote (" State_Path", State_Path));
--          Setups.Setups (Index) := Setup;
--          Setup.Setup_State := State;
--          State.Load (Location, State_Path, Video.Lib.Camera_ID_Type (Index));
--          Setup.Setup_Setup := Setup;
--          Setup.Load (State.all, Setup_Path);
--       end;
--    end loop;
-- end Load;

   ----------------------------------------------------------------
   procedure Load (
      Name              : in     String) is
   ----------------------------------------------------------------

      Config            : Ada_Lib.Configuration.Configuration_Type;
      Current_Directory : constant String :=
                           Standard.Camera.Lib.Options.Current_Directory;
      Path              : constant String :=
                           (if Current_Directory'length > 0 then
                              Current_Directory & "/"
                           else
                              "") &
                           Name;

      -------------------------------------------------------------
      procedure Load_Camers is
      -------------------------------------------------------------

      begin
         for Camera_Index in 1 .. Number_Cameras loop
            declare
               Name  : constant String := "camera_" & Trim (Camera_Index'img);
               Path  : constant String := Config.Get_String (Name);

            begin
               State.Load_Camera (Camera_Index, Path);
            end;
         end loop;
      end Load_Camers;

      -------------------------------------------------------------
      procedure Load_Configurations is
      -------------------------------------------------------------

      begin
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
                     Preset_ID      : constant Video.Lib.Preset_ID_Type :=
                                       Video.Lib.Preset_ID_Type (Iterator.Get_Number);
                     Label          : constant String := (if Iterator.At_End then
                                          ""
                                       else
                                          Iterator.Get_Value (Allow_Null => True));
                  begin
                     Log_Here (Debug, "configuration name " & Name &
                        " preset id " & Preset_ID'img &
                        Quote (" label", Label));

                     Configuration.Create (Configuration_ID,
                        Label    => Label,
                        Preset_ID=> Preset_ID);
                  end;
               end if;
            end;
         end loop;
      end Load_Configurations;

      -------------------------------------------------------------
      procedure Load_Presets is
      -------------------------------------------------------------

      begin
      -- lookup preset names in config
      for Preset_Number in 1 .. Number_Presets loop
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
                  Camera_Index   : constant Video.Lib.Camera_ID_Type :=
                                    Video.Lib.Camera_ID_Type (Iterator.Get_Number (
                                       Do_Next => True));
                  Row            : constant Row_Type :=
                                    Row_Type (Iterator.Get_Number (
                                       Do_Next => True));
                  Column         : constant Column_Type :=
                                    Column_Type (Iterator.Get_Number(
                                       Do_Next => True));
                  Camera_State   : constant State.State_Access :=
                                    State.Get_Camera_State (Camera_Index);

               begin
                  Log_Here (Debug, "preset" & Preset_Number'img &
                     Quote (" value", Value) &
                     " row" & Row'img & " column" & Column'img);

                  if Column > Camera_state.Get_Number_Columns then
                     raise Failed with "invalid column" & Column'img &
                        " maximum column" & Camera_state.Get_Number_Columns'img;
                  end if;
                  if Row > Camera_state.Get_Number_Rows then
                     raise Failed with "invalid row" & Row'img &
                        " maximum row" & Camera_state.Get_Number_Rows'img;
                  end if;

                  -- put preset in to presets array
                  Standard.Camera.Preset.Create_Preset (
                     Camera         => Camera_State.Get_Camera,
                     Camera_Index   => Camera_Index,
                     Column         => Column,
                     Preset_ID      => Preset_Number,
                     Row            => Row);
                  Log_Here (Debug, Standard.Camera.Preset.Image (Preset_Number));
               end;
--          else  -- if preset not defined set it to the null preset definition
--             Log_Here (Debug, "preset id" & Preset_Number'img);
--             Setup.Presets (Preset_Number) := Null_Preset;
            end if;
         end;
         Log_Here (Debug);
      end loop;
      end Load_Presets;
      -------------------------------------------------------------

--    Last_Preset_ID_Range
--                      : constant Video.Lib.Preset_Range_Type :=
--                         Last_Preset_ID.Get_ID;
   begin
      Log_In (Debug, Quote ("file name", Name) &
         Quote (" Current_Directory", Current_Directory) &
         Quote (" path", Path));

--    Global_Camera_Setup := Setup'unchecked_access;
      Config.Load (Path, False);

      Number_Cameras := Video.Lib.Camera_ID_Type (Config.Get_Integer (
                              "number_cameras"));
      Number_Configurations := Configuration.Configuration_ID_Type (Config.Get_Integer (
                              "maximum_configurations"));
      Number_Presets := Video.Lib.Preset_ID_Type (
                                    Config.Get_Integer ("maximum_presets"));
      -- allocate array for configurations
--    Configurations := new Configurations_Type (1 ..
--       Number_Configurations);
      Standard.Camera.Preset.Allocate_Presets (Number_Presets);

      Load_Camers;
      Load_Configurations;
      Load_Presets;


      Config.Close;
      Loaded := True;
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

-- ----------------------------------------------------------------
-- function Make_Image_Name (
--    Row                           : in     Row_Type;
--    Column                        : in     Column_Type
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return "image_" & Trim (Row'img) & "-" &Trim (Column'img);
-- end Make_Image_Name;

-- ----------------------------------------------------------------
-- function Make_Image_Name (
--    Preset                     : in     Preset_Type
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return Make_Image_Name (Preset.Row, Preset.Column);
-- end Make_Image_Name;

-- ----------------------------------------------------------------
-- function Get_Preset_Column (
--    Preset                     : in     Video.Lib.Preset_ID_Type
-- ) return Column_Type is
-- ----------------------------------------------------------------
--
-- begin
--    if Setup.Presets (Preset.Get_ID).Row = Row_Not_Set then
--       raise Failed with "preset" & Preset'img & " not defined";
--    else
--       return Setup.Presets (Preset.Get_ID).Column;
--    end if;
-- end Get_Preset_Column;
--
-- ----------------------------------------------------------------
-- function Preset_Image (
--    Preset                     : in     Preset_Type
-- ) return String is
-- ----------------------------------------------------------------
--
-- begin
--    return (if Preset.Row = Row_Not_Set then
--          "not set"
--       else
--          Trim (Preset.Column'img) &
--             "," & Trim (Preset.Row'img));
-- end Preset_Image;

-- ----------------------------------------------------------------
-- function Preset_Row (
--    Preset                     : in     Video.Lib.Preset_ID_Type
-- ) return Row_Type is
-- ----------------------------------------------------------------
--
-- begin
--    if Setup.Presets (Preset.Get_ID).Row = Row_Not_Set then
--       raise Failed with "preset" & Preset'img & " not defined";
--    else
--       return Setup.Presets (Preset.Get_ID).Row;
--    end if;
-- end Preset_Row;

   ----------------------------------------------------------------
   procedure Set_Path (
      Path                       : in     String) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, Quote ("path", Path));
      Configuration_Path.Construct (Path);
   end Set_Path;

   ----------------------------------------------------------------
   procedure Unload is
   ----------------------------------------------------------------

   begin
not_implemented;
   end Unload;

-- ----------------------------------------------------------------
-- procedure Unload (
--    State                      : in     Configuration.State.State_Type'class;
--    Save_Changes               : in     Boolean) is
-- ----------------------------------------------------------------
--
-- begin
--    Log_In (Debug, Setup.Is_Loaded'img &
--       " Save_Changes " & Save_Changes'img &
--       " modified " & Modified'img);
--
--    if Setup.Is_Loaded then
--       if Save_Changes and then Modified then
--          Setup.Update (State);
--          Modified := False;
--       end if;
--       Loaded := False;
--       Standard.Camera.Delete_Presets;
--    end if;
--    Log_Out (Debug);
--
-- exception
--
--    when Fault: others =>
--       Trace_Exception (Debug, Fault);
--       Log_Exception (Debug);
--       raise;
--
-- end Unload;

   ----------------------------------------------------------------
   procedure Update is
   ----------------------------------------------------------------

   begin
      if Modified then
         declare
            Config            : Ada_Lib.Configuration.Configuration_Type;
            Current_Directory : constant String :=
                                 Standard.Camera.Lib.Options.Current_Directory;
         begin
            Log_In (Debug,
               Quote ("Current_Directory", Current_Directory) &
               Configuration_Path.Quote (" path"));

            Config.Load (Configuration_Path.Coerce, Create => False);
            Camera.Preset.Update (Config);
            Configuration.Update (Config);
            Config.Store (Configuration_Path.Coerce);
         end;
      end if;
      Log_Out (Debug);

   exception

      when Fault: Failed =>
         Trace_Message_Exception (Debug, Fault,
            Configuration_Path.Quote ("with setup path"));
         Put_Line (Quote ("Load configuration setup", Configuration_Path.Coerce) &
            " failed with " & Ada.Exceptions.Exception_Message (Fault));
         Log_Exception (Debug);
         raise;

      when Fault: others =>
         Trace_Message_Exception (Debug, Fault,
            Configuration_Path.Quote ("with setup path"));
         Log_Exception (Debug);
         raise;

   end Update;

     ----------------------------------------------------------------
     procedure Update_Configuration (
        Configuration_ID           : in     Configuration.Configuration_ID_Type;
        Preset_ID                  : in     Video.Lib.Preset_ID_Type) is
     ----------------------------------------------------------------

     begin
        Log_Here (Debug, "Configuration" & Configuration_ID'img &
           " Preset_ID" & Preset_ID'img);

        Configuration.Update_Preset_ID (Configuration_ID, Preset_ID);
        Modified := True;
     end Update_Configuration;

     ----------------------------------------------------------------
     procedure Update_Configuration (
        Configuration_ID           : in     Configuration.Configuration_ID_Type;
        Label                      : in     String) is
     ----------------------------------------------------------------

     begin
        Log_Here (Debug, "Configuration" & Configuration_ID'img &
           Quote (" Label", Label));

        Configuration.Update_Label (Configuration_ID, Label);
        Modified := True;
     end Update_Configuration;

-- ----------------------------------------------------------------
-- procedure Update_Setup (
--    Setup_ID           : in     Setup_ID_Type;
--    Preset_ID                  : in     Video.Lib.Preset_ID_Type) is
-- ----------------------------------------------------------------
--
--    Setup              : Setup_Type renames
--                                  Setup.Setups (
--                                     Setup_ID);
-- begin
--    Log_Here (Debug, "Setup" & Setup_ID'img &
--       " Preset_ID" & Preset_ID.Image);
--
--    Setup.Preset_ID := Preset_ID;
--    Modified := True;
-- end Update_Setup;

-- ----------------------------------------------------------------
-- procedure Update_Setup (
--    Setup_ID           : in     Setup_ID_Type;
--    Label                      : in     String) is
-- ----------------------------------------------------------------
--
--    Setup              : Setup_Type renames
--                                  Setup.Setups (
--                                     Setup_ID);
-- begin
--    Log_Here (Debug, "Setup" & Setup_ID'img &
--       Quote (" Label", Label));
--
--    Setup.Label.Construct (Label);
--    Modified := True;
-- end Update_Setup;

-- ----------------------------------------------------------------
-- procedure Update_Preset (
--    Preset_ID                  : in     Video.Lib.Preset_ID_Type;
--    Row                        : in     Row_Type;
--    Column                     : in     Column_Type) is
-- ----------------------------------------------------------------
--
--    Preset                     : Preset_Type renames
--                                  Setup.Presets (Preset_Id.Get_ID);
-- begin
--    Log_Here (Debug, "preset id" & Preset_ID.Image &
--       " row" & Row'img & " column" & Column'img);
--    Preset.Column := Column;
--    Preset.Row := Row;
--    Setup.Modified := True;
-- end Update_Preset;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Elaborate);

end Configuration.Setup;
