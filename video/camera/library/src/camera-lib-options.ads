with ADA_LIB.Command_Line_Iterator;
--with Ada_Lib.Options.Nested;
with Ada_Lib.Options.Program;
with ADA_LIB.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Gnoga.Gui.Base;

package Camera.Lib.Options is

   Failed                        : Exception;

-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;
-- use type Ada_Lib.Options.Flags.Program_Options_Class_Access;

   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   type Source_Iterator_Type     is new Ada_Lib.Command_Line_Iterator.
                                    Internal.Iterator_Type with record
      Window                     : Gnoga.Gui.Base.Pointer_To_Base_Class;
   end record;

   -- type used for application options
   type Camera_Lib_Options_Nested_Options_Type(
      Multi_Test        : Boolean
   ) is limited new Camera_Lib_Nested_Options_Type (Multi_Test) with record
      Configuration_Path         : Ada_Lib.Strings.Unlimited.String_Type;
      Setup_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
      State_Path                 : Ada_Lib.Strings.Unlimited.String_Type;
--    Debug                      : Boolean := False;
      Template                   : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Camera_Lib_Options_Nested_Options_Access    is access all Camera_Lib_Options_Nested_Options_Type;
   type Camera_Lib_Options_Nested_Options_Class_Access
                                 is access all Camera_Lib_Options_Nested_Options_Type'class;
   type Camera_Lib_Options_Nested_Options_Constant_Class_Access
                                 is access constant Camera_Lib_Options_Nested_Options_Type'class;

   function Get_Camera_Lib_Options_Read_Only_Nested_Options (
      From                       : in     String := Ada_Lib.Trace.Here
   ) return Camera_Lib_Options_Nested_Options_Constant_Class_Access
   with Pre    => Ada_Lib.Options.Verification.Have_Ada_Lib_Verification_Options;

   overriding
   function Image (
     Options                     : in     Camera_Lib_Options_Nested_Options_Type
   ) return String;

   overriding
   function Initialize (
     Options                     : in out Camera_Lib_Options_Nested_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre    => not Options.Verify_Step (Initialized),
        post   => Options.Verify_Step (Initialized);

-- overriding
-- function Process_Option (  -- process one option
--    Options  : in out Camera_Lib_Options_Nested_Options_Type;
--    Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
--    Option   : in     Ada_Lib.Options.Flag_Option_Type'class
-- ) return Boolean
-- with pre => Options.Verify_Step (Initialized);
--
-- overriding
-- procedure Program_Help (
--    Options                    : in     Camera_Lib_Options_Nested_Options_Type;  -- only used for dispatch
--    Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   type Program_Options_Type (
      Multi_Test        : Boolean
   ) is limited new Ada_Lib.Options.Program.
                                    Program_Options_Type with record
      Camera_Lib_Options_Nested_Options
                                 : aliased Camera_Lib_Options_Nested_Options_Type (
                                    Multi_Test);
      Camera_State_Path          : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Program_Options_Access   is access all Program_Options_Type;
   type Program_Options_Class_Access
                                 is access all Program_Options_Type'class;
   type Program_Options_Constant_Class_Access
                                 is access constant Program_Options_Type'class;

   function Current_Directory -- set by runstring option 'c' else null
   return String
   with Pre => Have_Options;

   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre    => not Options.Verify_Step (Initialized),
        post   => Options.Verify_Step (Initialized);

   overriding
   function Process_Option (  -- process one option
      Options  : in out Program_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Step (Initialized);

   overriding
   procedure Program_Help (
      Options                    : in     Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   package Camera_Options is
      AUnit_Debug                : Boolean := False;
      Base_Debug                 : Boolean := False;
      Base_Lib_Debug             : Boolean := False;
      Camera_Control_Debug       : Boolean := False;
      Camera_Debug               : Boolean := False;
      Commands_Debug             : Boolean := False;
      Library_Debug              : Boolean := False;
      Hex_Debug                  : Boolean := False;
      Main_Debug                 : Boolean := False;
      Options_Debug              : Boolean := False;
      State_Debug                : Boolean := False;
      States_Debug               : Boolean := False;
   end Camera_Options;

-- package Camera_Main is
--    Debug                      : Boolean := False;
-- end Camera_Main;
--
   package Configuration_Options is
      Setup_Debug                : aliased Boolean := False;
      State_Debug                : aliased Boolean := False;
   end Configuration_Options;
private

end Camera.Lib.Options;
