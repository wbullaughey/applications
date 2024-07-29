with Ada_Lib.Unit_Test.Test_Cases;
with Ada_Lib.GNOGA.Unit_Test; -- .Base;
with Ada_Lib.Options.GNOGA;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace;
with AUnit.Ada_Lib.Options;
with AUnit.Options;
with AUnit.Simple_Test_Cases;
with AUnit.Test_Results;
with AUnit.Test_Suites;
with Camera.Commands;
with Camera.LIB.ALPTOP;
with Camera.Commands.PTZ_Optics;
with Configuration.Camera.Setup;
with Configuration.State;
with GNAT.Source_Info;
with Gnoga.GUI.Window;

package Camera.Lib.Unit_Test is

   Failed                        : exception;

   -- use for tests with camera but no web pages
   type Camera_Test_Type (
      Brand                      :Standard.Camera.Lib.Brand_Type) is abstract new
                                    Ada_Lib.Unit_Test.Test_Cases.
                                       Test_Case_Type with record
      Camera                     : Standard.Camera.Commands.
                                    Camera_Class_Access := Null;
      Camera_Address             : Address_Constant_Access := Null;
      Port_Number                : Port_Type;
      Location                   : Configuration.State.Location_Type;
      Setup                      : Configuration.Camera.Setup.Setup_Type;
      case Brand is
         when Standard.Camera.Lib.ALPTOP_Camera =>
            ALPTOP                : aliased Standard.Camera.LIB.ALPTOP.ALPTOP_Type;

         when Standard.Camera.Lib.No_Camera=>
            Null;

         when Standard.Camera.LIB.PTZ_Optics_Camera =>
            PTZ_Optics           : aliased Standard.Camera.Commands.
                                    PTZ_Optics.PTZ_Optics_Type;

      end case;
   end record;

   type Camera_Test_Access       is access Camera_Test_Type;
   type Camera_Test_Constant_Access
                                 is access constant Camera_Test_Type;
   overriding
   procedure Set_Up (
      Test                       : in out Camera_Test_Type
   ) with Pre => not Test.Verify_Set_Up,
          Post => Test.Verify_Set_Up;

   procedure Set_Up_Optional_Load (
      Test                       : in out Camera_Test_Type;
      Load                       : in     Boolean);

   overriding
   procedure Tear_Down (
      Test                       : in out Camera_Test_Type);

   -- use for test which create the standard main window which don't manipulate camera
   type Camera_Window_Test_Type (
      Initialize_GNOGA           : Boolean) is abstract new Ada_Lib.GNOGA.
                                    Unit_Test.GNOGA_Tests_Type (
                                       Initialize_GNOGA  => Initialize_GNOGA,
                                       Test_Driver       => False) with record
      Main_Window                : Gnoga.GUI.Window.Window_Type;
      Setup                      : Configuration.Camera.Setup.Setup_Type;
   end record;

   overriding
   procedure Set_Up (
      Test                       : in out Camera_Window_Test_Type
   ) with Pre => not Test.Verify_Set_Up,
          Post => Test.Verify_Set_Up;

   -- use for test which create the standard main window which manipulate camera
   type Camera_Window_Test_With_Camera_Type (
      Brand                      : Standard.Camera.Lib.Brand_Type;
      Initialize_GNOGA           : Boolean) is abstract new
                                    Camera_Window_Test_Type (
                                       Initialize_GNOGA) with null record;

   -- allocated options for unit test of camera library
   type Unit_Test_Options_Type   is new Options_Type
                                    with record
      AUnit_Options              : AUnit.Ada_Lib.Options.AUnit_Options_Type;
--    Camera_Options             : aliased Standard.Camera.Lib.Options_Type;
      GNOGA_Options              : Ada_Lib.Options.GNOGA.GNOGA_Options_Type;
--    GNOGA_Unit_Test_Options    : Ada_Lib.GNOGA.Unit_Test.
--                                  GNOGA_Unit_Test_Options_Type;F
      Main_Debug                 : Boolean := False;
      Unit_Test                  : Ada_Lib.Options.Unit_Test.
                                    Ada_Lib_Unit_Test_Options_Type (False);
   end record;

   type Unit_Test_Options_Access           is access all Camera_Lib_Unit_Test_Options_Type;
   type Unit_Test_Options_Class_Access     is access all Camera_Lib_Unit_Test_Options_Type'class;
   type Unit_Test_Options_Constant_Class_Access
                                 is access constant Camera_Lib_Unit_Test_Options_Type'class;
   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   function Get_Options (
      From              : in     String := GNAT.Source_Info.Source_Location
   ) return Unit_Test_Options_Constant_Class_Access;

   function Initialize
   return Boolean;

   overriding
   function Initialize (
     Options                     : in out Camera_Lib_Unit_Test_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

-- replaced with Camera.Lib.Get_Options
-- function Options (
--    From                    : in     String := Standard.GNAT.Source_Info.
--                                        Source_Location
-- ) return Unit_Test_Options_Constant_Class_Access;

   overriding
   function Process_Option (  -- process one option
      Options              : in out Camera_Lib_Unit_Test_Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class;
      Option               : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with Pre => Options.Initialized;
--             not Ada_Lib.Options.Have_Options;

   procedure Run_Suite (
     Options                    : in   Camera_Lib_Unit_Test_Options_Type);

   overriding
   procedure Trace_Parse (
      Options                    : in out Camera_Lib_Unit_Test_Options_Type;
      Iterator             : in out Ada_Lib.Options.
                                       Command_Line_Iterator_Interface'class
   ) with Pre => Options.Initialized and then
                 Ada_Lib.Options.Have_Options;

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

   Debug                         : Boolean := False;
   Debug_Options                 : Boolean := False;
   Unit_Test_Options             : Unit_Test_Options_Constant_Class_Access := Null;

private

   overriding
   procedure Program_Help (
      Options                    : in     Camera_Lib_Unit_Test_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type
   ) with Pre => Options.Initialized and then
                 Ada_Lib.Options.Have_Options;

end Camera.Lib.Unit_Test;
