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

   -- options common to all camera library routines
   type Camera_lib_Options_Type is limited new Video.Lib.Video_Lib_Options_Type
                                    with record
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

   type Camera_Lib_Options_Access
                                 is access all Camera_lib_Options_Type;
   type Camera_Lib_Options_Class_Access
                                 is access all Camera_lib_Options_Type'class;
   type Camera_Lib_Options_Constant_Class_Access
                                 is access constant
                                    Camera_lib_Options_Type'class;

   function Check_Options (
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   function Current_Directory -- set by runstring option 'c' else null
   return String
   with Pre => Camera.Lib.Check_Options;

   function Get_Modifiable_Camera_Lib_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Lib_Options_Class_Access
   with Pre => Check_Options;

   function Get_Read_Only_Camera_Lib_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Lib_Options_Constant_Class_Access
   with Pre => Check_Options;

   overriding
   function Initialize (
      Options               : in out Camera_lib_Options_Type;
      From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
      Options              : in out Camera_lib_Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class;
      Option               : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   overriding
   procedure Trace_Parse (
      Options                    : in out Camera_lib_Options_Type;
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

   function Hex is new Hex_IO.Modular_Hex (Value_Type);

   Debug                         : aliased Boolean := False;
   Debug_Options                 : aliased Boolean := False;
   Number_Configurations         : constant String := "configurations";
   Number_Grid_Columns           : constant String := "grid_columns";
   Number_Grid_Rows              : constant String := "grid_rows";

private

   overriding
   procedure Program_Help (
      Options                    : in     Camera_lib_Options_Type;
                                             -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Camera.Lib;
