with ADA_LIB.Command_Line_Iterator;
--with Ada_Lib.Configuration;
--with Ada_Lib.Options.Flags;
with Ada_Lib.Options.Nested;
with Hex_IO;
with Gnoga.Gui.Base;
with Video.Lib;

package Camera.Lib is

   Failed                        : exception;

   subtype Abstract_Iterator_Type
                                 is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   type Library_Options_Type     is new Video.Lib.Options_Type with record
      Camera_Options             : Camera_Options_Type;
      Lib_Debug                  : Boolean := False;
   end record;

   type Library_Options_Class_Access
                                 is access all Library_Options_Type'class;
   type Library_Options_Constant_Class_Access
                                 is access constant Library_Options_Type'class;


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

   function Get_Camera_Modifiable_Options
   return Library_Options_Class_Access
   with Pre => Have_Options and then
               Ada_Lib.Options.Nested.Have_Ada_Lib_Nested_Options;

   function Get_Camera_Readonly_Options
   return Library_Options_Constant_Class_Access
   with Pre => Have_Options and then
               Ada_Lib.Options.Nested.Have_Ada_Lib_Nested_Options;

   function Have_Options
   return Boolean;

   overriding
   function Initialize (
      Options              : in out Library_Options_Type;
      From                 : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
      Options  : in out Library_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   overriding
   procedure Trace_Parse (
      Options                    : in out Library_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class);

   type General_Camera_Type      is abstract new Video.Lib.Camera_Type
                                    with null record;
   type General_Camera_Class_Access
                                 is access all General_Camera_Type'class;

   overriding
   procedure Open (
      Camera                     :    out General_Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type);

   function Hex is new Hex_IO.Modular_Hex (Value_Type);

-- function Options (
--    From                       : in     String :=
--                                           Standard.GNAT.Source_Info.Source_Location
-- ) return Camera.Options_Constant_Class_Access;

   Number_Configurations         : constant String := "configurations";
   Number_Grid_Columns           : constant String := "grid_columns";
   Number_Grid_Rows              : constant String := "grid_rows";

private

   overriding
   procedure Program_Help (
      Options                    : in     Library_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Camera.Lib;
