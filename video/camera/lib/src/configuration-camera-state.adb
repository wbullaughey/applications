with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Configuration;
with Ada_Lib.Directory;
with Ada_Lib.Options.Actual;
--with Ada_Lib.Parser;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings.Unlimited; use Ada_Lib.Strings; use Ada_Lib.Strings.Unlimited;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with AUnit.Assertions; use AUnit.Assertions;
with Camera.Lib.Options;
--with Hex_IO;
with Video.Lib;

package body Configuration.Camera.State is

   procedure Free is new Ada.Unchecked_Deallocation (
      Images_Type,
      Images_Access);

   State_Pointer                 : State_Access := Null;

   ----------------------------------------------------------------
   function Check_Column (
      Column                     : in     Column_Type
   ) return Boolean is
   ----------------------------------------------------------------

      State             : Configuration.Camera.State.State_Type renames
                           Configuration.Camera.State.Get_Read_Only_State.all;
      Number_Columns    : constant Column_Type := State.Get_Number_Columns;

   begin
      return Log_Here (State.Images /= Null and then
         Column <= Number_Columns,  -- column number starts at 0
         Trace_Pre_Post_Conditions, "column" & Column'img &
            " number columns" & Number_Columns'img);
   end Check_Column;

   ----------------------------------------------------------------
   function Check_Image (
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (Check_Column (Column) and then Check_Row (Row),
         Trace_Pre_Post_Conditions);
   end Check_Image;

   ----------------------------------------------------------------
   function Check_Row (
      Row                        : in     Row_Type
   ) return Boolean is
   ----------------------------------------------------------------

      State       : Configuration.Camera.State.State_Type renames
                     Configuration.Camera.State.Get_Read_Only_State.all;
      Number_Rows : constant Row_Type := State.Get_Number_Rows;

   begin
      return Log_Here (State.Images /= Null and then
         Row <= State.Number_Rows, Trace_Pre_Post_Conditions,
         "row" & Row'img &
            " number rows" & Number_Rows'img);
   end Check_Row;

   ---------------------------------------------------------------
   procedure Clear_State is
   ---------------------------------------------------------------

   begin
      Log_Here (Debug);
      State_Pointer :=  Null;
   end Clear_State;

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
                                          Ada_Lib.Options.Actual.Get_Ada_Lib_Read_Only_Program_Options).
                                             Setup_Path;

   begin
      return (if State_Path.Length > 0 then
         State_Path.Coerce
      else
         Default_State);
   end File_Path;

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

      State       : Configuration.Camera.State.State_Type renames
                     Configuration.Camera.State.Get_Read_Only_State.all;
   begin
      return State.Default_Speed;
   end Get_Default_Speed;

   ----------------------------------------------------------------
   function Get_Modifiable_State return State_Access is
   ----------------------------------------------------------------

   begin
      return State_Pointer;
   end Get_Modifiable_State;

   ----------------------------------------------------------------
   function Get_Read_Only_State return State_Constant_Access is
   ----------------------------------------------------------------

   begin
      return State_Constant_Access (State_Pointer);
   end Get_Read_Only_State;

   ----------------------------------------------------------------
   function Get_Number_Columns (
      State                      : in     State_Type
   ) return Column_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Columns;
   end Get_Number_Columns;

   ----------------------------------------------------------------
   function Get_Number_Configurations (
      State                      : in     State_Type
   ) return Configuration_ID_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Configurations;
   end Get_Number_Configurations;

   ----------------------------------------------------------------
   function Get_Number_Presets (
      State                      : in     State_Type
   ) return Natural is
   ----------------------------------------------------------------

   begin
      return Natural (State.Get_Number_Columns);
   end Get_Number_Presets;

   ----------------------------------------------------------------
   function Get_Number_Rows (
      State                      : in     State_Type
   ) return Row_Type is
   ----------------------------------------------------------------

   begin
      return State.Number_Rows;
   end Get_Number_Rows;

-- ----------------------------------------------------------------
-- function Global_State_Is_Set
-- return Boolean is
-- ----------------------------------------------------------------
--
-- begin
--    return Global_Camera_State.Is_Loaded;
-- end Global_State_Is_Set;

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
      Column               : in     Configuration.Camera.Column_Type;
      Row                  : in     Configuration.Camera.Row_Type
   ) return String is
   ----------------------------------------------------------------

      State       : Configuration.Camera.State.State_Type renames
                     Configuration.Camera.State.Get_Read_Only_State.all;
      Image_Name  : constant String :=
                     State.Image_Path (Row, Column);
      Image_Path  : constant String := "img/" & Image_Name;
                     -- gnoga ads img/
   begin
      Log_Here (Debug, "row" & Row'img &
         " column" & Column'img &
         Quote (" image Name", Image_Name) &
         Quote (" image path", Image_Path));

      if Image_Name'length > 0 then
         if Ada_Lib.Directory.Exists (Image_Path) then
            return Image_Path;
         else
            raise Failed with Quote ("image path", Image_Path) & " does not exist";
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
      Last_Preset_Number         : Positive;
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
      State.CSS_Path.Construct (Config.Get_String ("css_path"));
      State.Default_Speed :=  Speed_Type (Config.Get_Integer (
         "default_speed"));
      State.Number_Columns := Column_Type (Config.Get_Integer (
         "grid_columns"));
      State.Number_Configurations := Configuration_ID_Type (
         Config.Get_Integer ("configurations"));
      State.Number_Rows := Row_Type (Config.Get_Integer ("grid_rows"));
      Last_Preset_Number := Config.Get_Integer ("last_preset");

--    declare
--       use Video.Lib;
--
--    begin
         Video.Lib.Set_Preset_ID (Video.Lib.Last_Preset,
            Video.Lib.Constructor (
            Video.Lib.Preset_Range_Type (Last_Preset_Number)));
--    end;

      Log_Here (Debug,
         Quote ("video address", State.Video_Address.Image) &
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
--Hex_IO.Dump_32 (State.Number_Columns'address, 32, 1, "number columns");
      Log_Out (Debug, "loaded " & State.Is_Loaded'img);

   exception

      when Fault: Ada_Lib.Configuration.Failed =>
         Trace_Exception (Debug, Fault);
         Put_Line ("Invalid configuration file: " &
            Ada.Exceptions.Exception_Message (Fault));
         raise;

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Load;

   ----------------------------------------------------------------
   function State_Set return Boolean is
   ----------------------------------------------------------------

   begin
      return Log_Here (State_Pointer /= Null, Debug or Trace_Pre_Post_Conditions);
   end State_Set;

   ----------------------------------------------------------------
   procedure Set_State (
      State                      : in     State_Access;
      From                       : in     String := Ada_Lib.Trace.Here) is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "from " & From);
      State_Pointer := State;
   end Set_State;

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
