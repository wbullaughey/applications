with ADA_LIB.Strings.Unlimited;
with Ada_Lib.Trace;
with Configuration.State;

package Configuration.Camera.State is

   type Images_Type              is array (Row_Type range <>,
                                       Column_Type range <>) of
                                          ADA_LIB.Strings.Unlimited.String_Type;
   type Images_Access            is access Images_Type;

   type State_Type               is new Configuration.State.State_Type with
                                    record
      CSS_Path                   : ADA_LIB.Strings.Unlimited.String_Type;
      Images                     : Images_Access := Null;
      Last_Preset                : Preset_ID_Type;
      Number_Columns             : Column_Type;
      Number_Configurations      : Configuration_ID_Type;
      Number_Rows                : Row_Type;
   end record;

   type State_Access             is access all State_Type;
   type State_Class_Access       is access all State_Type'class;
   type State_Constant_Access    is access constant State_Type;

   procedure Dump (
      State                      : in     State_Type;
      From                       : in     String := Ada_Lib.Trace.Here);

   function File_Path
   return String;

   function Global_State_Is_Set
   return Boolean;

   function Get_Number_Columns (
      State                      : in     State_Type
   ) return Column_Type;

   function Get_Number_Presets (
      State                      : in     State_Type
   ) return Preset_ID_Type;

   function Get_Number_Rows (
      State                      : in     State_Type
   ) return Row_Type;

   function Has_Image (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type
   ) return Boolean
   with Pre => State.Is_Set and then
               Global_Camera_State /= Null;

   function Image_Path (
      State                      : in     State_Type;
      Row                        : in     Row_Type;
      Column                     : in     Column_Type;
      Add_Prefix                 : in     Boolean := False
   ) return String
   with Pre => State.Is_Set and then
               Global_Camera_State /= Null and then
               Global_Camera_State.Images /= Null and then
               Row <= Global_Camera_State.Images.all'last (1) and then
               Column <= Global_Camera_State.Images.all'last (2);

   overriding
   function Is_Set (
      State                      : in     State_Type
   ) return Boolean;

   procedure Load_Camera_State (
      State                      : in out State_Type;
      Location                   : in     Configuration.State.Location_Type;
      Name                       : in     String
   ) with Pre => not State.Is_Set;

   overriding
   procedure Unload (
      State                      : in out State_Type);

   Default_State                 : constant String := "state.cfg";
   Global_Camera_State           : State_Access := Null;

end Configuration.Camera.State;
