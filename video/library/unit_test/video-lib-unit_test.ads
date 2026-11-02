with Ada_Lib.Options.AUnit_Lib;
with Ada_Lib.Options.Program;
with Ada_Lib.Options.Unit_Test;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;

package Video.Lib.Unit_Test is

   type Nested_Options_Type
                           is limited new Video.Lib.
                              Video_Lib_Nested_Options_Type with null record;

   type Aunit_Program_Options_Type (
      Multi_Test           : Boolean; -- perform multiple tests in one
      Options_Selection    : Ada_Lib.Options.AUnit_Lib.Options_Selection_Type
   ) is limited new
         Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (
            Multi_Test        => Multi_Test,
            Options_Selection => Ada_Lib.Options.AUnit_Lib.
                                    Unit_Test_With_No_Database_Or_Template
      ) with null record;
--       Nested_Unit_Test_Options
--                         : aliased Video.Lib.Video_Lib_Nested_Options_Type (Multi_Test);
-- end record;

   overriding
   function Initialize (
     Options                     : in out Aunit_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean;

   overriding
   function Process (     -- process command line options
     Options                  : in out Aunit_Program_Options_Type;
     Iterator                 : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean;

   overriding
   function Process_Option (  -- process one option
      Options     : in out Aunit_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean;

   overriding
   procedure Program_Help (
      Options                    : in     Aunit_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   procedure Run_Suite  (
      Options  : in     Aunit_Program_Options_Type);

   package Options_Package is
      Main_Debug                 : Boolean := False;
      Unit_Test_Debug            : Boolean := False;
   end Options_Package;

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Video.Lib.Unit_Test;

