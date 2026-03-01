with Ada_Lib.Configuration;
with ADA_LIB.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;
with Camera.Configurations;
with Configuration.State;

package Configuration.Camera.State is

   type Images_Type              is array (Row_Type range <>,
                                       Column_Type range <>) of
                                          ADA_LIB.Strings.Unlimited.String_Type;
   type Images_Access            is access Images_Type;

   type State_Type               is new Configuration.State.State_Type with
                                    private;

   type State_Access             is access all State_Type;
   type State_Class_Access       is access all State_Type'class;
   type State_Constant_Access    is access constant State_Type;
   type State_Constant_Class_Access
                                 is access constant State_Type'class;

   function Check_Column (
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => Standard.Camera.Configurations.Has_Configuration;

   function Check_Image (
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return Boolean;

   function Check_Row (
      Row                        : in     Row_Type
   ) return Boolean
   with Pre => Standard.Camera.Configurations.Has_Configuration;

   procedure Clear_Global_Camera_State (
      State                      : in out State_Type
   ) with Pre  => State.Has_Camera_ID,
          Post => not State.Has_Camera_ID;

   procedure Copy (
      Destination                : in out State_Type;
      Source                     : in     State_Type);

   procedure Dump (
      State                      : in     State_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

-- function File_Path
-- return String;

   function Get_Camera_ID (
      State                      : in     State_Type
   ) return Standard.Camera.Camera_ID_Type
   with Pre => State.Has_Camera_ID;

   function Get_Camera_Name (
      State                      : in     State_Type
   ) return String
   with Pre => State.Has_Camera_ID;

   function Get_CSS_Path (
      State                      : in     State_Type
   ) return String;

   function Get_Default_Speed
   return Speed_Type
   with Pre => Standard.Camera.Configurations.Has_Configuration;

   function Get_Modifiable_Global_State return State_Access
   with Pre    => Standard.Camera.Configurations.Has_Configuration;

-- overriding
   function Get_Number_Columns (
      State                      : in     State_Type'class
   ) return Column_Type;

-- overriding
   function Get_Number_Configurations (
      State                      : in     State_Type'class
   ) return Configuration_ID_Type;

-- overriding
   function Get_Number_Presets (
      State                      : in     State_Type'class
   ) return Natural;

-- overriding
   function Get_Number_Rows (
      State                      : in     State_Type'class
   ) return Row_Type;

   function Has_Camera_ID (
      State                      : in     State_Type
   ) return Boolean;

   function Has_Image (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => State.Is_Loaded and then
               Check_Image (Column, Row);

   function Image_Name (
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return String
   with Pre => Check_Column (Column) and then
               Check_Row (Row);

   function Image_Path (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type;
      Add_Prefix                 : in     Boolean := False
   ) return String
   with Pre => State.Is_Loaded and then
               Check_Image (
                  Column   => Column,
                  Row      => Row);

   overriding
   procedure Load (
      State       : in out State_Type;
      Config      : in out Ada_Lib.Configuration.Configuration_Type;
      Location    : in     Configuration.State.Location_Type;
      File_Name   : in     String
   ) with Pre => not State.Is_Loaded,
          Post => State.Is_Loaded and then
                  State.Have_Video_Address;

-- procedure Set_State (
--    State                      : in     State_Access;
--    From                       : in     String := Ada_Lib.Trace.Here
-- ) with Pre  => not Standard.Camera.Configurations.Has_Configuration,
--        Post => Standard.Camera.Configurations.Has_Configuration;

   overriding
   procedure Unload (
      State                      : in out State_Type);

   Default_State                 : constant String := "state.cfg";

private

   type State_Type            is new Configuration.State.State_Type with record
      Camera_ID               : Standard.Camera.Camera_ID_Type;
      Camera_Name             : Ada_Lib.Strings.Unlimited.String_Type;
      CSS_Path                : ADA_LIB.Strings.Unlimited.String_Type;
      Default_Speed           : Speed_Type :=
                                 (Speed_Type'last - Speed_Type'first)/2;
      Images                  : Images_Access := Null;
                                 -- pointer two dimensional array of image paths
                                 -- 1st dimension is row, second is column
      Number_Columns          : Column_Type;
      Number_Configurations   : Configuration_ID_Type;
      Number_Rows             : Row_Type;
   end record;


end Configuration.Camera.State;

