--with Ada_Lib.GNOGA;
with ADA_LIB.Strings.Unlimited;
with Ada_Lib.Trace;
with Camera;
with Configuration.State;
with GNOGA_Ada_Lib;

package Configuration.Camera.State is

   type Images_Type              is array (Row_Type range <>,
                                       Column_Type range <>) of
                                          ADA_LIB.Strings.Unlimited.String_Type;
   type Images_Access            is access Images_Type;

   type State_Type               is new Configuration.State.State_Type with
                                    record
      CSS_Path                   : ADA_LIB.Strings.Unlimited.String_Type;
      Default_Speed              : Speed_Type;
      Images                     : Images_Access := Null;
      Number_Columns             : Column_Type;
      Number_Configurations      : Configuration_ID_Type;
      Number_Rows                : Row_Type;
   end record;

   type State_Access             is access all State_Type;
   type State_Class_Access       is access all State_Type'class;
   type State_Constant_Access    is access constant State_Type;

   function Check_Column (
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => GNOGA_Ada_Lib.Has_Connection_Data;

   function Check_Image (
      Column                     : in     Column_Type;
      Row                        : in     Row_Type
   ) return Boolean;

   function Check_Row (
      Row                        : in     Row_Type
   ) return Boolean
   with Pre => GNOGA_Ada_Lib.Has_Connection_Data;

   procedure Dump (
      State                      : in     State_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

   function File_Path
   return String;

-- function Global_State_Is_Set
-- return Boolean;

   function Get_Default_Speed
   return Speed_Type
   with Pre => Gnoga_Ada_Lib.Has_Connection_Data;

   function Get_Number_Columns (
      State                      : in     State_Type
   ) return Column_Type;

   function Get_Number_Presets (
      State                      : in     State_Type
   ) return Natural;

   function Get_Number_Rows (
      State                      : in     State_Type
   ) return Row_Type;

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
               Check_Image (Column, Row);

-- overriding
-- function Is_Loaded (
--    State                      : in     State_Type
-- ) return Boolean;

   overriding
   procedure Load (
      State                      : in out State_Type;
      Location                   : in     Configuration.State.Location_Type;
      Name                       : in     String
   ) with Pre => not State.Is_Loaded,
          Post => State.Is_Loaded;

   overriding
   procedure Unload (
      State                      : in out State_Type);

   Default_State                 : constant String := "state.cfg";
-- Global_Camera_State           : State_Access := Null;

   Debug                         : Boolean := False;

end Configuration.Camera.State;

