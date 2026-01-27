with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Configuration;
with Ada_Lib.Directory;
with Ada_Lib.Options;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with AUnit.Assertions; use AUnit.Assertions;
with Camera.Lib.Options;
--with Camera.States;
with Video.Lib;

package body Configuration.Camera.State is

-- use type Standard.Camera.Camera_ID_Type;

   procedure Free is new Ada.Unchecked_Deallocation (
      Images_Type,
      Images_Access);

   Debug : Boolean renames Standard.Camera.Lib.Options.
            Configuration_Options.State_Debug;

   ----------------------------------------------------------------
   function Check_Column (
      Column                     : in     Column_Type
   ) return Boolean is
   ----------------------------------------------------------------

      State_Pointer  : constant Configuration.Camera.State.State_Constant_Access :=
                        Standard.Camera.States.Get_Read_Only_Configuration_State;
      State          : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
      Number_Columns : constant Column_Type := State.Get_Number_Columns;
      Result         : constant Boolean := State.Images /= Null and then
                        Column <= Number_Columns;  -- column number starts at 0
   begin
      return Log_Here (Result,
         Trace_Pre_Post_Conditions or not Result, "column" & Column'img &
            " number columns" & Number_Columns'img);
   end Check_Column;

   ----------------------------------------------------------------
   function Check_Image (
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return Boolean is
   ----------------------------------------------------------------

      Result         : constant Boolean := Check_Column (Column) and then
                                             Check_Row (Row);
   begin
      return Log_Here (Result,
         Trace_Pre_Post_Conditions or not Result,
         "no image for column" & Column'img &
            " row" & Row'img);
   end Check_Image;

   ----------------------------------------------------------------
   function Check_Row (
      Row                        : in     Row_Type
   ) return Boolean is
   ----------------------------------------------------------------

      State_Pointer  : constant Configuration.Camera.State.State_Constant_Access :=
                        Standard.Camera.States.Get_Read_Only_Configuration_State;
      State          : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
      Number_Rows : constant Row_Type := State.Get_Number_Rows;
      Result         : constant Boolean := State.Images /= Null and then
                                             Row <= State.Number_Rows;

   begin
      return Log_Here (Result, Trace_Pre_Post_Conditions or not Result,
         "row" & Row'img &
            " number rows" & Number_Rows'img);
   end Check_Row;

   ---------------------------------------------------------------
   procedure Clear_Global_Camera_State (
      State                      : in out State_Type) is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug);
      State.Camera_ID := Standard.Camera.Null_Camera_ID;
   end Clear_Global_Camera_State;

   ----------------------------------------------------------------
   procedure Copy (
      Destination                : in out State_Type;
      Source                     : in     State_Type) is
   ----------------------------------------------------------------

   begin
      Destination := Source;
   end Copy;

   ----------------------------------------------------------------
   procedure Dump (
      State                      : in     State_Type;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Put_Line ("dump Configuration state type from " & From);
      State.Dump (From);
      Put_Line (Quote ("  CSS Path", State.CSS_Path));
      Put_Line ("  Number Columns:" & State.Get_Number_Columns'img);
      Put_Line ("  Number Configurations:" & State.Get_Number_Configurations'img);
      Put_Line ("  Last Preset:" & Video.Lib.Get_Last_Preset_ID'img);
      Put_Line ("  Number Rows:" & State.Number_Rows'img);
      for Row in State.Images.all'range (1) loop
         for Column in State.Images.all'range (2) loop
            Put_Line ("row" & row'img & " column" & Column'img &
               Quote (" path", State.Images (Row, Column)));
         end loop;
      end loop;
   end Dump;

   ----------------------------------------------------------------
   function File_Path
   return String is
   ----------------------------------------------------------------

      State_Path                 : Ada_Lib.Strings.Unlimited.String_Type
                                    renames Standard.Camera.Lib.Options.
                                       Program_Options_Constant_Class_Access (
                                          Ada_Lib.Options.Get_Ada_Lib_Read_Only_Program_Options).
                                             Setup_Path;

   begin
      return (if State_Path.Length > 0 then
         State_Path.Coerce
      else
         Default_State);
   end File_Path;

   ----------------------------------------------------------------
   function Get_Camera_ID (
      State                      : in     State_Type
   ) return Standard.Camera.Camera_ID_Type is
   ----------------------------------------------------------------

   begin
      return State.Camera_ID;
   end Get_Camera_ID;

   ----------------------------------------------------------------
   function Get_Camera_Name (
      State                      : in     State_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return State.Camera_Name.Coerce;
   end Get_Camera_Name;

   ----------------------------------------------------------------
   function Get_CSS_Path (
      State                      : in     State_Type
   ) return String is
   ----------------------------------------------------------------

   begin
      return State.CSS_Path.Coerce;
   end Get_CSS_Path;

   ----------------------------------------------------------------
   function Get_Default_Speed
   return Speed_Type is
   ----------------------------------------------------------------

begin
log_here;

declare
      State_Pointer  : constant Configuration.Camera.State.State_Constant_Access :=
                        Standard.Camera.States.Get_Read_Only_Configuration_State;
      State          : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
   begin
      Log_Here (Debug, State.Default_Speed'img);
      return State.Default_Speed;
end;
   end Get_Default_Speed;

   ----------------------------------------------------------------
   function Get_Modifiable_Global_State return State_Access is
   ----------------------------------------------------------------

   begin
not_implemented;
return Null;
   end Get_Modifiable_Global_State;

   ----------------------------------------------------------------
   overriding
   function Get_Number_Columns (
      State                      : in     State_Type
   ) return Column_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Columns;
   end Get_Number_Columns;

   ----------------------------------------------------------------
   overriding
   function Get_Number_Configurations (
      State                      : in     State_Type
   ) return Configuration_ID_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Configurations;
   end Get_Number_Configurations;

   ----------------------------------------------------------------
   overriding
   function Get_Number_Presets (
      State                      : in     State_Type
   ) return Natural is
   ----------------------------------------------------------------

   begin
      return Natural (State.Get_Number_Columns);
   end Get_Number_Presets;

   ----------------------------------------------------------------
   overriding
   function Get_Number_Rows (
      State                      : in     State_Type
   ) return Row_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Rows;
   end Get_Number_Rows;

   ----------------------------------------------------------------
   function Has_Camera_ID (
      State                      : in     State_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State.Camera_ID.Is_Set,
         Debug or else Trace_Pre_Post_Conditions,
         "camera id" & State.Camera_ID'img);
   end Has_Camera_ID;

   ----------------------------------------------------------------
   function Has_Image (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
--log_here (row'img & column'img & (if State.Images = Null then " null images" else " have image"));
--log_here (state.images.all'last(1)'img & state.images.all'last(2)'img);
--Log_here ("state address " & image (state'address));
      return (if State.Images = Null then
         False
      else
         State.Images (Row, Column).Length > 0);
   end Has_Image;

   ----------------------------------------------------------------
   function Image_Name (
      Column               : in     Configuration.Column_Type;
      Row                  : in     Configuration.Row_Type
   ) return String is
   ----------------------------------------------------------------

      State_Pointer  : constant Configuration.Camera.State.State_Constant_Access :=
                        Standard.Camera.States.Get_Read_Only_Configuration_State;
      State          : Configuration.Camera.State.State_Type renames
                        State_Pointer.all;
      Name        : constant String :=
                     State.Image_Path (Row, Column);
      Path        : constant String := "img/" & Name;
                     -- gnoga ads img/
   begin
      Log_Here (Debug, "row" & Row'img &
         " column" & Column'img &
         Quote (" image Name", Name) &
         Quote (" image path", Path));

      if Name'length > 0 then
         if Ada_Lib.Directory.Exists (Path) then
            return Path;
         else
            raise Failed with Quote ("image path", Path) & " does not exist";
         end if;
      else
         return "";
      end if;
   end Image_Name;

   ----------------------------------------------------------------
   function Image_Path (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type;
      Add_Prefix                 : in     Boolean := False
   ) return String is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "row" & row'img & " column" & column'img &
         " add prefix " & Add_Prefix'img &
         (if State.Images = Null then " null images" else " have image"));


      declare
         Path     : constant String := State.Images (Row, Column).Coerce;
         Result   : constant String := (if Add_Prefix then
                        "img/"
                     else
                        "") &
                     (if Path = "" then
                        Blank_Preset_Root
                     else
                        Path);

      begin
         Log_Out (Debug, Quote ("result", Result));
         return Result;
      end;
   end Image_Path;

-- ----------------------------------------------------------------
-- overriding
-- function Is_Loaded (
--    State                      : in     State_Type
-- ) return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return State.Set;
-- end Is_Loaded;

   ----------------------------------------------------------------
   overriding
   procedure Load (
      State                      : in out State_Type;
      Location                   : in     Configuration.State.Location_Type;
      Name                       : in     String) is
   ----------------------------------------------------------------

      Config                     : Ada_Lib.Configuration.Configuration_Type;
      Current_Directory          : constant String :=
                                    Standard.Camera.Lib.Options.Current_Directory;
      Last_Preset_Number         : Video.Lib.Preset_Range_Type;
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
      Config.Load (Path, False);
      State.Load (Config, Location, Path);
      State.Camera_ID := Standard.Camera.Make_Camera_ID (State.Video_Address.all);
      State.Camera_Name.Construct (Config.Get_String ("camera_name"));
      State.CSS_Path.Construct (Config.Get_String ("css_path"));
      State.Default_Speed :=  Speed_Type (Config.Get_Integer (
         "default_speed"));
      State.Number_Columns := Column_Type (Config.Get_Integer (
         "grid_columns"));
      State.Number_Configurations := Configuration_ID_Type (
         Config.Get_Integer ("configurations"));
      State.Number_Rows := Row_Type (Config.Get_Integer ("grid_rows"));
      Last_Preset_Number := Video.Lib.Preset_Range_Type (
         Config.Get_Integer ("last_preset"));

      Video.Lib.Set_Preset_ID (Video.Lib.Last_Preset,
         Video.Lib.Constructor (Last_Preset_Number));

      Log_Here (Debug,
         "camera id " & State.Camera_ID.Image &
         Quote (" video address", State.Video_Address.Image) &
         " camera id" & State.Camera_ID.Image &
         Quote ("video port", State.Video_Port'img) &
         Quote (" CSS_Path", State.CSS_Path) &
         " Number_Columns" & State.Number_Columns'img &
         " Number_Configurations" & State.Get_Number_Configurations'img &
         " Last_Preset" & Last_Preset_Number'img &
         " Number_Rows" & State.Number_Rows'img);
      -- allocate 2 dimensional array of image file names
      State.Images := new Images_Type (1 .. State.Number_Rows,
         1 .. State.Number_Columns);

      -- lookup image names in configuration to get file names
      for Row in 1 .. State.Number_Rows loop
         for Column in 1 .. State.Number_Columns loop
            declare
               Name              : constant String :=
                                    "image_" & Trim (Row'img) & "-" &
                                    Trim (Column'img);
            begin
               Log_Here (Debug, "Row" & Row'img & " Column" & Column'img &
                  Quote (" name", Name));
               if Config.Has (Name) then
                  declare
                     Value       : constant String :=
                                    Config.Get_String  (Name);

                  begin
                     Log_Here (Debug, Quote ("value", Value));
--                      Quote (" path", Path));
                     State.Images (Row, Column).Construct (Value);
                  end;
               else  -- if file name not in config leave zero legnth file name
                  Log_Here (Debug, "row" & Row'img &
                     " column" & Column'img & " not configured");
               end if;
            end;
         end loop;
      end loop;

      Config.Close;
      State.Set_Loaded (True);
      Log_Out (Debug, "loaded " & State.Is_Loaded'img);

   exception

      when Fault: Ada_Lib.Configuration.Failed =>
         Trace_Exception (Debug, Fault);
         Put_Line ("Could not load configuration file: " &
            Ada.Exceptions.Exception_Message (Fault));
         raise;

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Load;

--   ----------------------------------------------------------------
--   procedure Set_State (
--      State                      : in     State_Access;
--      From                       : in     String := Ada_Lib.Trace.Here) is
--   ----------------------------------------------------------------
--
--   begin
--      Log_Here (Debug, "from " & From);
--not_implemented;
--   end Set_State;

   ----------------------------------------------------------------
   overriding
   procedure Unload (
      State                      : in out State_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "state set " & State.Is_Loaded'img);
      if State.Is_Loaded then
         State.Set_Loaded (False);
         Free (State.Images);
      end if;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, "exception " & Ada.Exceptions.Exception_Message (Fault));

   end Unload;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug or Trace_Options or Elaborate);

end Configuration.Camera.State;
