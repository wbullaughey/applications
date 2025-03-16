--with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Options.Actual;
with Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace;

package Driver is

   Failed                        : exception;

   type Driver_Options_Type (
         Testing                 : Boolean) is limited new Ada_Lib.Options.
                                    Actual.Nested_Options_Type with record
      Camera_Directory           : Ada_Lib.Strings.Unlimited.String_Type;
      Camera_Options             : Ada_Lib.Strings.Unlimited.String_Type;
      Driver_Debug               : Boolean := False;
      List_Output                : Boolean := False;
      Main_Debug                 : Boolean := False;
      Remote_Camera              : Boolean := False;
      Routine               : Ada_Lib.Strings.Unlimited.String_Type;
      Suite                 : Ada_Lib.Strings.Unlimited.String_Type;
   end record;

   type Driver_Options_Access    is access all Driver_Options_Type;
   type Driver_Options_Class_Access
                                 is access all Driver_Options_Type'class;
   type Driver_Options_Constant_Class_Access
                                 is access constant Driver_Options_Type'class;

   overriding
   function Initialize (
     Options                     : in out Driver_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
     Options                    : in out Driver_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                          Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   overriding
   procedure Trace_Parse (
      Options                    : in out Driver_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                          Command_Line_Iterator_Interface'class);

   type Program_Options_Type     is limited new Ada_Lib.Options.Actual.
                                    Program_Options_Type with record
      Driver_Options             : Driver_Options_Type (False);
   end record;

   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   overriding
   function Process_Option (  -- process one option
     Options                    : in out Program_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                          Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   overriding
   procedure Trace_Parse (
      Options        : in out Program_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class);

   function Get_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Options_Class_Access;

-- procedure Get_Suite_Routines (
--    Test_Suite                 : in     String);

   procedure Get_Tests;

   procedure Push (
      Suite                      : in     String;
      Routine                    : in     String);

   procedure Queue_Tests;

   procedure Run_Selection;

   procedure Set_Protected_Options (
      Options                    : in Driver_Options_Class_Access);

private

   overriding
   procedure Program_Help (
      Options                    : in     Driver_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   overriding
   procedure Program_Help (
      Options                    : in     Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Driver;
