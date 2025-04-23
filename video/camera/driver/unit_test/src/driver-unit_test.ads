with Ada_Lib.Unit_Test;
with Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
with AUnit.Test_Suites;
with Camera.Lib.Unit_Test;

package Driver.Unit_Test is

   Failed                        : exception;

   type Driver_Test_Type         is new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type
                                    with null record;
   type Driver_Test_Access       is access Driver_Test_Type;
   type Driver_Test_Class_Access is access Driver_Test_Type'class;

   type Driver_Unit_Test_Options_Type is limited new Camera.Lib.Unit_Test.
         Unit_Test_Program_Options_Type with record
--    Camera_Options             : Ada_Lib.Strings.Unlimited.String_Type;
      Driver_Options             : aliased Driver_Options_Type (True);
--    Unit_Test_Options          : aliased Ada_Lib.Options.Unit_Test.
--                                     Unit_Test_Program_Options_Type (True);
   end record;

   type Driver_Unit_Test_Option_Access
                                 is access all Driver_Unit_Test_Options_Type;
   type Driver_Unit_Test_Option_Class_Access
                                 is access all Driver_Unit_Test_Options_Type'class;
   type Driver_Unit_Test_Option_Constant_Class_Access
                                 is access constant Driver_Unit_Test_Options_Type'class;

   function Initialize
   return Boolean;

   overriding
   function Process_Option (  -- process one option
     Options                    : in out Driver_Unit_Test_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   function Get_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Unit_Test_Option_Class_Access;

   function Get_Readonly_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Unit_Test_Option_Constant_Class_Access;

   overriding
   function Name (
      Test                       : in     Driver_Test_Type
   ) return AUnit.Message_String;

   procedure Run_Suite;

   Suite_Name                    : constant String := "Driver";

private

   function Create_Suite return AUnit.Test_Suites.Access_Test_Suite
   with Pre => Ada_Lib.Options.Actual.Have_Program_Options;

   overriding
   procedure Program_Help (
      Options                    : in     Driver_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

   overriding
   procedure Register_Tests (
      Test                       : in out Driver_Test_Type);

end Driver.Unit_Test;
