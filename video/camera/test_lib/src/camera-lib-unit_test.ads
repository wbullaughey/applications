with Ada_Lib.Options.Actual;
with Ada_Lib.Options.AUnit_Lib;
with Ada_Lib.GNOGA.Unit_Test; -- .Base;
--with GNOGA_Options;
with Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
--with AUnit.Ada_Lib.Options;
with AUnit.Options;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Results;
with AUnit.Test_Suites;
with Camera.Commands;
--with Camera.LIB.ALPTOP;
with Camera.Commands.PTZ_Optics;
with Configuration.Camera.Setup;
with Configuration.State;
with GNAT.Source_Info;
with Gnoga.GUI.Window;
with Gnoga_Ada_Lib;

package Camera.Lib.Unit_Test is

   Failed               : exception;

   use for all camera tests
   type Camera_Test_Interface is Interface;

   type Camera_Info_Type   is record
      Camera               : Standard.Camera.Commands.
                              Camera_Class_Access := Null;
      Camera_Address       : Address_Constant_Access := Null;
      Location             : Configuration.State.Location_Type;
      Open_Camera          : Boolean := True;
      Port_Number          : Port_Type := Standard.Camera.Commands.PTZ_Optics.Port;
   end record;

   -- use for all tests
   type Test_Type is abstract new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type
         with null record;

   -- use for tests with camera but no web pages
   type With_Camera_No_GNOGA_Test_Type (
      Brand             : Standard.Camera.Lib.Brand_Type
   ) is abstract new Test_Type and Camera_Test_Interface with record
      Camera_Info       : Camera_Info_Type;
      Load_State        : Boolean := True;
      Setup             : Configuration.Camera.Setup.Setup_Type;
      Setup_Path        : access constant String := Null;
      State_Path        : access constant String := Null;
   end record;

   type With_Camera_No_GNOGA_Test_Access
                        is access With_Camera_No_GNOGA_Test_Type;
   type With_Camera_No_GNOGA_Constant_Test_Access
                        is access constant With_Camera_No_GNOGA_Test_Type;
   procedure Check_Preset (
      Test                       : in     With_Camera_No_GNOGA_Test_Type);

   procedure Dump (
      Test                       : in     With_Camera_No_GNOGA_Test_Type;
      Trace                      : in     Boolean);

   function Have_Camera (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   function Have_Camera_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   function Have_Video_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   procedure Load_Test_State (
      Test                       : in out With_Camera_No_GNOGA_Test_Type
   ) with Pre => Gnoga_Ada_Lib.Has_Connection_Data;

   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_No_GNOGA_Test_Type
   ) with Pre  => not Test.Have_Camera and then
                  not Test.Setup.Is_Loaded,
          Post => Test.Verify_Set_Up and then
                  Test.Have_Camera and then
                  (  if Test.Load_State then
                        Test.Setup.Is_Loaded
                     else
                        True
                  );
   overriding
   procedure Tear_Down (
      Test                       : in out With_Camera_No_GNOGA_Test_Type);

   -- use for test which create the standard main window
   -- which don't manipulate camera
   type No_Camera_With_GNOGA_Test_Type (
      Initialize_GNOGA           : Boolean)
                              is abstract new Ada_Lib.GNOGA.GNOGA_Interface
                                 with record
      GNOGA_Test              : Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (
                                       Initialize_GNOGA  => Initialize_GNOGA,
                                       Test_Driver       => False);
      Main_Window             : Gnoga.GUI.Window.Window_Type;
      Setup                   : Configuration.Camera.Setup.Setup_Type;
      Unit_Test               : Ada_Lib.Unit_Test.Tests.Test_Case_Type;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out No_Camera_With_GNOGA_Test_Type
   ) with Post => Test.Verify_Set_Up;

   -- use for test which create the standard main window which manipulate camera
   type With_Camera_With_GNOGA_Test_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type;
      Initialize_GNOGA           : Boolean) is abstract new
                                    No_Camera_With_GNOGA_Test_Type (
                                       Initialize_GNOGA) with record
      Camera_Info                : Camera_Info_Type;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_With_GNOGA_Test_Type
   ) with Post => Test.Verify_Set_Up;

   -- allocated options for unit test of camera library
   type Unit_Test_Program_Options_Type is new
      Ada_Lib.Options.AUnit_Lib.Aunit_Program_Options_Type (
         Multi_Test           => False,
         Options_Selection    => Ada_Lib.Options.AUnit_Lib.
                                    Not_Ada_Lib_Unit_Test) with record
         -- camera unit tests only can be run one test per invokation
      Camera_Options             : aliased Standard.Camera.Lib.Options_Type;
--    GNOGA_Options              : GNOGA_Options.GNOGA_Options_Type;
--    GNOGA_Unit_Test_Options    : Ada_Lib.GNOGA.Unit_Test.
--                                  GNOGA_Unit_Test_Options_Type;F
      Main_Debug                 : Boolean := False;
--    Unit_Test                  : Ada_Lib.Options.Unit_Test.
--                                  Ada_Lib_Unit_Test_Program_Options_Type (False);
   end record;

   type Unit_Test_Options_Access           is access all Unit_Test_Program_Options_Type;
   type Unit_Test_Options_Class_Access     is access all Unit_Test_Program_Options_Type'class;
   type Unit_Test_Options_Constant_Class_Access
                                 is access constant Unit_Test_Program_Options_Type'class;
   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   function Get_Camera_Unit_Test_Constant_Options (
      From                    : in     String := Standard.GNAT.Source_Info.
                                          Source_Location
   ) return Unit_Test_Options_Constant_Class_Access;


   overriding
   function Initialize (
     Options                     : in out Unit_Test_Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   overriding
   function Process_Option (  -- process one option
     Options                     : in out Unit_Test_Program_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean
   with Pre => Options.Initialized;
--             not Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;

   procedure Run_Suite (
     Options                    : in   Unit_Test_Program_Options_Type);

   overriding
   procedure Trace_Parse (
      Options     : in out Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                     Command_Line_Iterator_Interface'class
   ) with Pre => Options.Initialized and then
                 Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;

   type Test_Suite is new AUnit.Test_Suites.Test_Suite with null record;

   procedure Add_Test (
      Suite                      : access Test_Suite'Class;
      Test                       : access AUnit.Simple_Test_Cases.Test_Case'Class);

   overriding
   procedure Run (
      Suite                      : access Test_Suite;
      Options                    :        AUnit.Options.AUnit_Options;
      Results                    : in out AUnit.Test_Results.Result'Class;
      Outcome                    :    out AUnit.Status);

   Camera_Commands_Debug         : Boolean := False;
   Debug                         : Boolean := False;
   Unit_Test_Options             : Unit_Test_Options_Constant_Class_Access := Null;

private

   overriding
   procedure Program_Help (
      Options                    : in     Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type
   ) with Pre => Options.Initialized and then
                 Ada_Lib.Options.Actual.Have_Ada_Lib_Program_Options;

end Camera.Lib.Unit_Test;
