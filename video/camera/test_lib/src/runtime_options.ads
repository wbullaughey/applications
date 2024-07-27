with ADA_LIB.Command_Line_Iterator;
with ADA_LIB.Options;
--with Ada_Lib.Options.GNOGA;
--with ADA_LIB.Strings.Unlimited;
with ADA_LIB.Trace;
--with Ada_Lib.Socket_IO;
with Gnoga.Gui.Base;
with Camera.Lib;

package Runtime_Options is

   Failed                        : Exception;

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   type Source_Iterator_Type     is new Ada_Lib.Command_Line_Iterator.
                                    Internal.Iterator_Type with record
      Window                     : Gnoga.Gui.Base.Pointer_To_Base_Class;
   end record;

   type Options_Type             is limited new Camera.Lib.
                                    Options_Type with null record;

   type Options_Access           is access all Options_Type;
   type Options_Class_Access     is access all Options_Type'class;
   type Options_Constant_Class_Access
                                 is access constant Options_Type'class;

-- function Current_Directory -- set by runstring option 'c' else null
-- return String
-- with Pre => Ada_Lib.Options.Get_Read_Only_Options /= Null;
--
-- function Get_Modifiable_Options return Options_Access;

   overriding
   function Initialize (
     Options                     : in out Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

-- function Process (
--   Options                     : in out Options_Type
-- ) return Boolean;

   overriding
   function Process_Option (  -- process one option
     Options                    : in out Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class;
      Option               : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;
-- with Pre => not Ada_Lib.Options.Have_Options;

   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class);

   Debug                         : aliased Boolean := False;  -- not set as option

private

-- overriding
-- procedure Process (     -- processes whole command line calling Process_Option for each option
--   Options                    : in out Options_Type;
--   Iterator                   : in out ADA_LIB.Command_Line_Iterator.
--                                  Abstract_Package.Abstract_Iterator_Type'class);

   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   procedure Set_Options (
      Options                    : in     Options_Class_Access;
      From                       : in     String := Ada_Lib.Trace.Here);

end Runtime_Options;
