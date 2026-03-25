with Ada_Lib.GNOGA.Unit_Test; -- .Base;
with Ada_Lib.Options.Program;
with Ada_Lib.Options.Verification;
with Ada_Lib.Trace;
with Ada_Lib.Unit_Test.Test_Cases;
--with AUnit.Simple_Test_Cases;
--with AUnit.Test_Cases;
with AUnit.Test_Suites;
with Camera.Base;
with Camera.Commands;
with Camera.Lib.Options;
--with Camera.Configuration;
--with Camera.Configurations;
with Configuration.Camera.Setup;
--with Configuration.Camera.State;
with GNAT.Source_Info;
with Gnoga.Application.Multi_Connect;
--with Video.Lib;

package Camera.Lib.Unit_Test is

   use type Camera.Commands.Camera_Class_Access;
-- use type Address_Constant_Access;
-- use type Port_Type;
-- use type Video.Lib.Location_Type;

   Failed               : exception;

---- use for all camera tests
--   type Camera_Test_Interface is limited Interface;

   type Camera_Info_Type   is record
      Camera               : Standard.Camera.Commands.
                              Camera_Class_Access := Null;
      Camera_Options       : Options.Camera_Lib_Options_Nested_Options_Type;
      Open_Camera          : Boolean := True;
   end record;

--   procedure Load_Test_State (
--      Configuration      : in out Standard.Camera.Base.Configuration_Type;
--      Camera_Info       : in out Camera_Info_Type;
--      Setup             : in out Standard.Configuration.Camera.Setup.Setup_Type
----    State             : in out Configuration.Camera.State.State_Type
--   ) with Pre  => Camera_Info.Camera /= Null,
--          Post => Camera_Info.Camera_Options.Camera_Address /= Null and then
--                  Camera_Info.Camera_Options.Port_Number /= Video.Lib.Port_Type'last and then
--                  Camera.Configurations.Has_Camera_ID (Make_Camera_ID (
--                     Camera_Info.Camera_Options.Camera_Address.all)) and then
--                  Setup.Is_Loaded;

   -- use for tests with camera but no web pages
   type With_Camera_No_GNOGA_Test_Type
         is abstract new Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type with record
      Brand                : Brand_Type := PTZ_Optics_Camera;
      Camera_Info          : Camera_Info_Type;
--    Camera_State         : Standard.Configuration.Camera.State.State_Type;
      Configuration        : Standard.Camera.Base.Configuration_Type;
--    Configuration_Setup  : Standard.Configuration.Camera.Setup.Setup_Type;
--    Configuration_State  : Standard.Configuration.Camera.State.State_Type;
      Load_State           : Boolean := True;
      Setup_Path           : access constant String := Null;
      State_Path           : access constant String := Null;
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

   function Get_Camera_ID (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Camera_ID_Type;

   function Have_Camera (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   function Have_Camera_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   function Have_Video_Address (
      Test                       : in     With_Camera_No_GNOGA_Test_Type
   ) return Boolean;

   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_No_GNOGA_Test_Type
   ) with Pre  => not Test.Have_Camera, --  and then
--                not Test.Configuration.Get_Configuration_Setup.Is_Loaded,
          Post => Test.Verify_Set_Up and then
                  ( if Test.Load_State then
                        Test.Have_Camera and then
                        Test.Configuration.Get_Configuration_Setup.Is_Loaded
                     else
                        True);
   overriding
   procedure Tear_Down (
      Test                       : in out With_Camera_No_GNOGA_Test_Type
   ) with Post => Test.Verify_Tear_Down;

   type Camera_Lib_GNOGA_Test_Type (
      Initialize_GNOGA     : Boolean) is abstract new
                              Ada_Lib.GNOGA.Unit_Test.GNOGA_Tests_Type (
                                 Initialize_GNOGA  => Initialize_GNOGA,
                                 Test_Driver       => False) with record
--    Camera_State         : aliased State.State_Type;
      Configuration        : aliased Standard.Camera.Base.Configuration_Type;
--    Configuration_Setup  : Standard.Configuration.Camera.Setup.Setup_Type;
--    Configuration_State  : Standard.Configuration.Camera.State.State_Type;
      Load_State           : Boolean := True;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out Camera_Lib_GNOGA_Test_Type
   ) with Post => Test.Verify_Set_Up;

   overriding
   procedure Set_Up_With_Handler (
      Test           : in out Camera_Lib_GNOGA_Test_Type;
      Test_Handler   : in     Gnoga.Application.Multi_Connect.
                                 Application_Connect_Event;
      Wait_For_Message_Loop_Exit
                     : in     Boolean);

   overriding
   procedure Tear_Down (
      Test                       : in out Camera_Lib_GNOGA_Test_Type
   ) with Post => Test.Verify_Tear_Down;

   -- use for test which create the standard main window
   -- which don't manipulate camera
   type No_Camera_With_GNOGA_Test_Type (
      Initialize_GNOGA           : Boolean)
                              is abstract new Camera_Lib_GNOGA_Test_Type (
                                       Initialize_GNOGA  => Initialize_GNOGA)
                                    with null record;

   -- use for test which create the standard main window which manipulate camera
   type With_Camera_With_GNOGA_Test_Type (
      Initialize_GNOGA           : Boolean) is abstract new
                                    Camera_Lib_GNOGA_Test_Type (
                                       Initialize_GNOGA) with record
      Brand                      : Brand_Type := PTZ_Optics_Camera;
      Camera_Info                : Camera_Info_Type;
      Setup                      : Standard.Configuration.Camera.Setup.Setup_Type;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out With_Camera_With_GNOGA_Test_Type
   ) with Post => Test.Verify_Set_Up;

-- procedure Set_Up_With_Handler (
--    Test           : in out With_Camera_With_GNOGA_Test_Type;
--    Test_Handler   : in     Gnoga.Application.Multi_Connect.
--                               Application_Connect_Event;
--    Wait_For_Message_Loop_Exit
--                   : in     Boolean);

   -- allocated options for unit test of camera library
   type Camera_Lib_Unit_Test_Program_Options_Type is limited new
      Ada_Lib.Options.Program.Program_Options_Type with record
         -- camera unit tests only can be run one test per invokation
--    Nested_Options : aliased Options.Camera_Lib_Options_Nested_Options_Type;
      Main_Debug     : Boolean := False;
   end record;

   type Camera_Lib_Unit_Test_Program_Options_Access
      is access all Camera_Lib_Unit_Test_Program_Options_Type;
   type Camera_Lib_Unit_Test_Program_Options_Class_Access
      is access all Camera_Lib_Unit_Test_Program_Options_Type'class;
   type Camera_Lib_Unit_Test_Program_Options_Constant_Class_Access
      is access constant Camera_Lib_Unit_Test_Program_Options_Type'class;
   subtype Runtime_Iterator_Type
                     is Ada_Lib.Command_Line_Iterator.
                        Abstract_Package.Abstract_Iterator_Type;

   function Get_Camera_Unit_Test_Constant_Options (
      From           : in     String := Standard.GNAT.Source_Info.
                                          Source_Location
   ) return Camera_Lib_Unit_Test_Program_Options_Constant_Class_Access;

-- function Get_Configuration_Path
-- return String;

   overriding
   function Initialize (
     Options                     : in out Camera_Lib_Unit_Test_Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre    => Options.Verify_Preinitialize,
        post   => Options.Verify_Initialized;

   overriding
   function Process_Option (  -- process one option
      Options  : in out Camera_Lib_Unit_Test_Program_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean
   with Pre => Options.Verify_Initialized;
--             not Ada_Lib.Options.Verification.Have_Ada_Lib_Program_Options;

-- procedure Run_Suite (
--    Options  : Options.Unit_Test.Camera_Unit_Test_Program_Options_Type
-- ) with Pre => Ada_Lib.Options.Verification.Have_Ada_Lib_Program_Options;

   overriding
   procedure Trace_Parse (
      Options     : in out Camera_Lib_Unit_Test_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                     Command_Line_Iterator_Interface'class
   ) with Pre => Options.Verify_Initialized and then
                 Ada_Lib.Options.Verification.Have_Ada_Lib_Program_Options;

   type Camera_Test_Suite is new AUnit.Test_Suites.Test_Suite with null record;

-- function Has_Camera
-- return Boolean
-- with Pre    => Ada_Lib.Options.Verification.Have_Ada_Lib_Program_Options;

   procedure Setup_Camera (
      Load_State     : in     Boolean;
      Brand          : in     Standard.Camera.Brand_Type;
      Camera_Info    : in out Camera_Info_Type;
      Configuration  : in out Standard.Camera.Base.Configuration_Type);

   Camera_Commands_Debug         : Boolean := False;
-- Unit_Test_Options             : Unit_Test_Options_Constant_Class_Access := Null;

private

   overriding
   procedure Program_Help (
      Options                    : in     Camera_Lib_Unit_Test_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type
   ) with Pre => Options.Verify_Initialized and then
                 Ada_Lib.Options.Verification.Have_Ada_Lib_Program_Options;

end Camera.Lib.Unit_Test;
