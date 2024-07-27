with ADA_LIB.Command_Line_Iterator;
--with Ada_Lib.Options.Actual;
--with Ada_Lib.Configuration;
with Ada_Lib.Options.GNOGA;
with ADA_LIB.Strings.Unlimited;
--with GNAT.Source_Info;
with Hex_IO;
with Gnoga.Gui.Base;
with Video.Lib;

package Camera.Lib is

   Failed                        : exception;

   type Brand_Type               is (ALPTOP_Camera, PTZ_Optics_Camera, No_Camera);

   type Source_Iterator_Type     is new Ada_Lib.Command_Line_Iterator.
                                    Internal.Iterator_Type with record
      Window                     : Gnoga.Gui.Base.Pointer_To_Base_Class;
   end record;

   procedure Initialize (
      Iterator                   :    out Source_Iterator_Type;
      Window                     : in     Gnoga.Gui.Base.Pointer_To_Base_Class;
      Source                     : in     String;
      Include_Options            : in     Boolean;
      Include_Non_Options        : in     Boolean;
      Argument_Seperator         : in     Character := ' ';
      Option_Prefix              : in     Character := '-';
      Skip                       : in     Natural := 0);

   type Options_Type is limited new Video.Lib.Options_Type with
                                    record
      Brand                      : Brand_Type := PTZ_Optics_Camera;
      Debug                      : Boolean := False;
      Directory                  : ADA_LIB.Strings.Unlimited.String_Type;
                                    -- set by runstring option 'c'
      GNOGA                      : Ada_Lib.Options.GNOGA.GNOGA_Options_Type;
      Lib_Debug                  : Boolean := False;
      Setup_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
      Simulate                   : Boolean := False;
      State_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
      Template                   : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Options_Access           is access all Options_Type;
   type Options_Class_Access     is access all Options_Type'class;
   type Options_Constant_Class_Access
                                 is access constant Options_Type'class;

   function Check_Options (
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   function Current_Directory -- set by runstring option 'c' else null
   return String
   with Pre => Ada_Lib.Options.Have_Options;

   function Get_Modifiable_Options return Options_Class_Access
   with Pre => Check_Options;
   function Get_Options return Options_Constant_Class_Access
   with Pre => Check_Options;

   overriding
   function Initialize (
      Options               : in out Options_Type;
      From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
      Options              : in out Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class;
      Option               : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class);

   type General_Camera_Type      is abstract new Camera_Type
                                    with null record;
   type General_Camera_Class_Access
                                 is access all General_Camera_Type'class;

   overriding
   procedure Open (
      Camera                     :    out General_Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type);

   function Camera_Options
   return Options_Constant_Class_Access;

   function Hex is new Hex_IO.Modular_Hex (Value_Type);

-- function Options (
--    From                       : in     String :=
--                                           Standard.GNAT.Source_Info.Source_Location
-- ) return Options_Constant_Class_Access;

   Debug                         : aliased Boolean := False;
   Debug_Options                 : aliased Boolean := False;
   Number_Configurations         : constant String := "configurations";
   Number_Grid_Columns           : constant String := "grid_columns";
   Number_Grid_Rows              : constant String := "grid_rows";

private

   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Camera.Lib;
