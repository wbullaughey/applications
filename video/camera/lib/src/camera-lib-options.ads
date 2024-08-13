with ADA_LIB.Command_Line_Iterator;
with ADA_LIB.Options.Actual;
with ADA_LIB.Options.GNOGA;
--with Ada_Lib.Options;
with ADA_LIB.Strings.Unlimited;
--with ADA_LIB.Trace;
--with Ada_Lib.Socket_IO;
with Gnoga.Gui.Base;
--with Camera.Lib;

package Camera.Lib.Options is

   Failed                        : Exception;

   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   type Source_Iterator_Type     is new Ada_Lib.Command_Line_Iterator.
                                    Internal.Iterator_Type with record
      Window                     : Gnoga.Gui.Base.Pointer_To_Base_Class;
   end record;

   -- options for camera_control application
   type Camera_Options_Type      is limited new Ada_Lib.Options.Actual.
                                    Program_Options_Type with record
      Camera_Library             : aliased Camera.Lib.Camera_Lib_Options_Type;
      Setup_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
      State_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
      Debug                      : Boolean := False;
      GNOGA                      : Ada_Lib.Options.GNOGA.GNOGA_Options_Type;
      Template                   : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Camera_Options_Access    is access all Camera_Options_Type;
   type Camera_Options_Class_Access
                                 is access all Camera_Options_Type'class;
   type Camera_Options_Constant_Class_Access
                                 is access constant Camera_Options_Type'class;

   overriding
   function Initialize (
     Options                     : in out Camera_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

-- function Process (
--   Options                     : in out Camera_Options_Type
-- ) return Boolean;

   overriding
   function Process_Option (  -- process one option
      Options           : in out Camera_Options_Type;
      Iterator          : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option            : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;
-- with Pre => not Ada_Lib.Options.Have_Options;

   Debug                         : aliased Boolean := False;  -- not set as option

private

-- overriding
-- procedure Process (     -- processes whole command line calling Process_Option for each option
--   Options                    : in out Camera_Options_Type;
--   Iterator                   : in out ADA_LIB.Command_Line_Iterator.
--                                  Abstract_Package.Abstract_Iterator_Type'class);

   overriding
   procedure Program_Help (
      Options                    : in     Camera_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Camera.Lib.Options;
