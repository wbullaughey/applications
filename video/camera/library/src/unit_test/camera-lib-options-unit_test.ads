with Ada_Lib.Options.Program;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Unit_Test.Test_Cases;
with Camera.Lib.Unit_Test;

package Camera.Lib.Options.Unit_Test is

   type Camera_Unit_Test_Program_Options_Type (
      Multi_Test  : Boolean -- perform multiple tests in one
                                            -- execution of test program
         ) is new Ada_Lib.Options.Program.Program_Options_Type with record
      Camera_Lib_Nested_Options
                  : Camera_Lib_Options_Nested_Options_Type;
      Camera_Lib_Unit_Test_Program_Options
                  : Camera.Lib.Unit_Test.Camera_Lib_Unit_Test_Program_Options_Type;
      Nested_Unit_Test_Options
                  : aliased Ada_Lib.Options.Unit_Test.
                     Ada_Lib_Unit_Test_Nested_Options_Type (
                        Multi_Test => True);
   end record;

   type Camera_Unit_Test_Program_Options_Class_Access
                                 is access all Camera_Unit_Test_Program_Options_Type'class;
   type Camera_Unit_Test_Program_Options_Constant_Class_Access
                                 is access constant Camera_Unit_Test_Program_Options_Type'class;

   Failure                       : exception;

   overriding
   procedure Display_Help (
                              -- prints full help, aborts program
     Options   : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
     Message   : in     String := "";   -- leave blank no error help
     Halt      : in     Boolean := True);

   function Get_Modifiable_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Class_Access;
-- with pre => Ada_Lib.Options.Have_Options;

   function Get_Read_Only_Camera_Unit_Test_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Camera_Unit_Test_Program_Options_Constant_Class_Access;
-- with pre => Ada_Lib.Options.Have_Options;

   function Image (
     Options                     : in     Camera_Unit_Test_Program_Options_Type
   ) return String;

   overriding
   function Initialize (
     Options                     : in out Camera_Unit_Test_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean
   with pre    => Options.Verify_Preinitialize,
        Post   => Options.Verify_Initialized;

   overriding
   function Process (     -- process command line options
     Options   : in out Camera_Unit_Test_Program_Options_Type;
     Iterator  : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean;

-- procedure Set_Options;

-- AUnit_Lib_Options             : Aunit_Program_Options_Constant_Class_Access := Null;

   package Camera_Lib_Unit_Test is
      Adjust_Debug               : Boolean := False;
      Base_Debug                 : Boolean := False;
      Configuration_Setup_Debug  : Boolean := False;
      Configuration_State_Debug  : Boolean := False;
      Unit_Test_Debug            : Boolean := False;
   end Camera_Lib_Unit_Test;

   package Camera_Main_Unit_Test is
      Debug               : Boolean := False;
   end Camera_Main_Unit_Test;

private

   overriding
   procedure Program_Help (
      Options     : in     Camera_Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode   : in     Ada_Lib.Options.Help_Mode_Type);

   overriding
   function Process_Option (
      Options     : in out Camera_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean
   with pre => Options.Verify_Initialized;

   procedure Register_Tests (
      Options     : in     Camera_Unit_Test_Program_Options_Type;
      Suite_Name  : in     String;
      Test        : in out Ada_Lib.Unit_Test.Test_Cases.
                              Test_Case_Type'class);
   overriding
   procedure Trace_Parse (
      Options     : in out Camera_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                              Command_Line_Iterator_Interface'class);

end Camera.Lib.Options.Unit_Test;

