with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options.Program;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.OS;
with Ada_Lib.String_Quote; use Ada_Lib.String_Quote;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Options.Verification;
with Ada_Lib.Unit_Test.Reporter;
with Ada_Lib.Unit_Test.Test_Cases;
with Aunit.Assertions; use Aunit.Assertions;
with AUnit.Options;
with AUnit.Test_Results;
--with AUnit.Test_Suites;
with AUnit.Test_Cases;

package body Video.Lib.Unit_Test is

-- use type Ada_Lib.Options.Flag_List_Type;
-- use type Ada_Lib.Options.Flag_Option_Kind_Type;
   use type Ada_Lib.Options.Mode_Type;

   type Test_Camera_Type is new Camera_Type with null record;

   overriding
   procedure Close (
      Camera                     : in out Test_Camera_Type);

   procedure Host_Open (
      Camera                     :    out Test_Camera_Type;
      Host_Address               : in     String;
      Port                       : in     Port_Type);

   procedure Get_Absolute (
      Camera                     : in out Test_Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time);

   function Image (
      Camera                     : in     Test_Camera_Type
   ) return String;

   procedure IP_Open (
      Camera                     :    out Test_Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     Port_Type);

   procedure Open (
      Camera                     :     out Test_Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type);

   procedure Relative_Command (
      Camera                     : in out Test_Camera_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Preset (
      Camera                     : in out Test_Camera_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True;
      Speed                      : in     Property_Type := 0);
                                               -- 0 => default
   procedure URL_Open (
      Camera                     :    out Test_Camera_Type;
      URL                        : in     String;
      Port                       : in     Port_Type);

   type Test_Type    is new Ada_Lib.Unit_Test.Test_Cases.
                        Test_Case_Type with null record;

   type Test_Access  is access all Test_Type;

   overriding
   function Name (
      Test           : Test_Type
   ) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test           : in out Test_Type);

   procedure Test_Has_Camera  (
      Test           : in out AUnit.Test_Cases.Test_Case'class);

   Debug             : Boolean renames Options_Package.Unit_Test_Debug;
   Initialize_Recursed
                     : Boolean := False;

   Trace_Option      : constant Character := 'v';
   Options_With_Parameters
                     : aliased constant Ada_Lib.Options.Flag_List_Type :=
                        Ada_Lib.Options.Initialize (
                           Trace_Option, Ada_Lib.Help.Modifier);
   Suite_Name        : constant String := "Video_Lib_Test";

   ----------------------------------------------------------------------------
   overriding
   procedure Close (
      Camera                     : in out Test_Camera_Type) is
   ----------------------------------------------------------------------------

   begin
      null;
   end Close;

   ----------------------------------------------------------------------------
   procedure Get_Absolute (
      Camera                     : in out Test_Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time) is
   ----------------------------------------------------------------------------

   begin
      null;
   end Get_Absolute;

   ----------------------------------------------------------------------------
   overriding
   function Process (
      Options                    : in out Aunit_Program_Options_Type;
      Iterator                   : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Log_It   : constant Boolean := Trace_Options;

   begin
      Log_In (Log_It);

      while not Iterator.At_End loop
         begin
            if Iterator.Is_Option then
               declare
                  Option         : constant Ada_Lib.Options.Flag_Option_Type'class :=
                                    Iterator.Get_Option;
                  Message        : constant String := Option.Image & " not defined";

               begin
                  Log_Here (Log_It, Option.Image &
                     " Options_Selection " & Options.Options_Selection'img);
                  if    Trace_Return (Log_It,
                           Options.Nested_Unit_Test_Options.Process_Option (
                                 Iterator, Option),
                              "Nested_Unit_Test_Options" & Option.Image) or else
                        Trace_Return (Log_It,
                           Options.Process_Option (Iterator, Option),
                           "options") then
                     Log_Here (Log_It, Option.Image, "processed");
                  else
                     Log_Here (Log_It, Message);
                     Options.Bad_Option (Option, Message);     -- aborts program
                     return Log_Out (False, Log_It);
                  end if;
               end;
            else
               declare
                  Argument          : constant String :=
                                       Iterator.Get_Argument;
               begin
                  Log_Out (Log_It);
                  Options.Bad_Option ("unexpected '" & Argument & "' on run string" &
                     " from " & Here);
               end;
            end if;

         end;
         if not Iterator.At_End then
            Iterator.Advance;
         end if;
      end loop;

      return Log_Out (Ada_Lib.Options.Program.Program_Options_Type (
         Options).Process (Iterator), Log_It, "processed");

   exception

      when Fault: others =>
         Trace_Exception (Log_It, Fault);
         raise;

   end Process;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Aunit_Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Video Lib";
      Log_It                     : constant Boolean := Debug or Trace_Options;

   begin
      Log_In (Log_It, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program_Mode =>
         Log_Here (Log_It, Quote ("Component", Component));

         Ada_Lib.Help.Create_Option (Trace_Option, True, "trace option",
            "trace", Component, Ada_Lib.Help.Modifier);
         New_Line;

      when Ada_Lib.Options.Trace_Mode =>
         Ada_Lib.Help.Set_Has_Trace (Trace_Option, Ada_Lib.Help.Modifier);
         New_Line;

         Put_Line (Component & " trace options (-" &
            Ada_Lib.Help.Modifier & Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      m               Unit_Test" &
            " (Video.Lib.Unit_Test.Options_Package.Main_Debug");
         Put_Line ("      u               Unit_Test" &
            " (Video.Lib.Unit_Test.Options_Package.Unit_Test_Debug");
         New_Line;

      end case;

      Options.Nested_Unit_Test_Options.Program_Help (Help_Mode);
--    Ada_Lib.Options.Program.Program_Options_Type (Options).Program_Help (Help_Mode);
      Log_Out (Log_It);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Host_Open (
      Camera                     :    out Test_Camera_Type;
      Host_Address               : in     String;
      Port                       : in     Port_Type) is
   ----------------------------------------------------------------------------

   begin
      null;
   end Host_Open;

   ----------------------------------------------------------------------------
   function Image (
      Camera                     : in     Test_Camera_Type
   ) return String is
   ----------------------------------------------------------------------------

   begin
      return "";
   end Image;

   ----------------------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Aunit_Program_Options_Type;
     From                        : in     String := Standard.Ada_Lib.Trace.Here
   ) return Boolean is
   ----------------------------------------------------------------------------

      Log_It   : constant Boolean := Trace_Options;

   begin
      Log_In_Checked (Initialize_Recursed, Log_It);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);

      return Log_Out_Checked (Initialize_Recursed,
         Options.Nested_Unit_Test_Options.Initialize and then
         Ada_Lib.Options.Program.Program_Options_Type (
            Options).Initialize,
         Log_It);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure IP_Open (
      Camera                     :    out Test_Camera_Type;
      IP_Address                 : in     GNAT.Sockets.Inet_Addr_V4_Type;
      Port                       : in     Port_Type) is
   ----------------------------------------------------------------------------

   begin
      null;
   end IP_Open;

   ----------------------------------------------------------------------------
   procedure Open (
      Camera                     :     out Test_Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type) is
   ----------------------------------------------------------------------------

   begin
      null;
   end Open;

   ---------------------------------------------------------------
   overriding
   function Name (
      Test           : Test_Type
   ) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

   ----------------------------------------------------------------------------
   overriding
   function Process_Option (  -- process one option
      Options     : in out Aunit_Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

      Log_It   : constant Boolean := Trace_Options;

   begin
      Log_In (Log_It, "option '" & Option.Image &
         " kind " & Option.Kind'img);
--       " Help_Test " & Options.Help_Test'img);

      if Option.Has_Option (Options_With_Parameters,
            Ada_Lib.Options.Null_Flag_List) then
         case Option.Kind is

            when Ada_Lib.Options.Plain =>
               case Option.Option is

                  when 'a' =>
                     Options.Trace_Parse (Iterator);

                  when Others =>
                     Log_Exception (Log_It);
                     raise Failed with "Has_Option incorrectly passed " &
                        Option.Image;

               end case;

            when Ada_Lib.Options.Modified =>
               case Option.Option is

                  when 'v' =>
                     Options_Package.Unit_Test_Debug := True;  -- Debug renamed

                  when Others =>
                     Log_Exception (Log_It);
                     raise Failed with "Has_Option incorrectly passed " &
                        Option.Image;

               end case;

            when Ada_Lib.Options. Nil_Option =>
               pragma Assert (False, "unexpected kind");

         end case;
      else
         return Log_Out (False, Log_It, Quote (" option not handled",
            Option.Image));
      end if;
      return Log_Out (True, Log_It, Quote (" option handled", Option.Image));

   end Process_Option;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test           : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Has_Camera'access,
         Routine_Name   => AUnit.Format ("Test_Has_Camera")));

      Log_Out (Debug);
   end Register_Tests;

   ----------------------------------------------------------------------------
   procedure Relative_Command (
      Camera                     : in out Test_Camera_Type;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is
   ----------------------------------------------------------------------------

   begin
      null;
   end Relative_Command;

   ----------------------------------------------------------------------------
   procedure Run_Suite  (
      Options  : in     Aunit_Program_Options_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);

      declare
         AUnit_Options           : AUnit.Options.AUnit_Options;
         Outcome                 : AUnit.Status;
         Reporter                : Ada_Lib.Unit_Test.Reporter.Reporter_Type;
         Results                 : AUnit.Test_Results.Result;
         Test_Suite              : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    (if Options.Nested_Unit_Test_Options.Mode =
                                          Ada_Lib.Options.List_Suites then
                                       new AUnit.Test_Suites.Test_Suite
                                    else
                                       AUnit.Test_Suites.New_Suite);
--       Tests                      : constant Test_Access := new Test_Type;
--
      begin
         AUnit_Options.Filter := Options.Nested_Unit_Test_Options.Filter'
                                    unchecked_access;

         Test_Suite.Add_Test (Suite);

         Test_Suite.Run (AUnit_Options, Results, Outcome);

         Log_Here (Debug, "Mode " & Options.Nested_Unit_Test_Options.Mode'img);
         case Options.Nested_Unit_Test_Options.Mode is

            when Ada_Lib.Options.Driver_Suites |
                 Ada_Lib.Options.List_Suites |
                 Ada_Lib.Options.Print_Suites =>
               Ada_Lib.Unit_Test.Iterate_Suites (
                  Ada_Lib.Options.Unit_Test.Suite_Action'access,
                  Ada_Lib.Options.Unit_Test.Routine_Action'access,
                  Options.Nested_Unit_Test_Options.Mode);

            when Ada_Lib.Options.Run_Tests =>
               Log_Here (Debug);
               Put_Line ("report Ada_Lib test results");
               Reporter.Report (Results, AUnit_Options);

         end case;
      end;
      Log_Out (Debug);
   end Run_Suite;

   ----------------------------------------------------------------------------
   procedure Set_Preset (
      Camera                     : in out Test_Camera_Type;
      Preset_ID                  : in     Video.Lib.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True;
      Speed                      : in     Property_Type := 0) is
   ----------------------------------------------------------------------------
                                               -- 0 => default
   begin
      null;
   end Set_Preset;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Tests                      : constant Test_Access := new Test_Type;

   begin
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Tests);
      return Test_Suite;
   end Suite;

   ----------------------------------------------------------------------------
   procedure Test_Has_Camera  (
      Test           : in out AUnit.Test_Cases.Test_Case'class) is
   pragma Unreferenced (Test);
   ----------------------------------------------------------------------------

      Camera         : aliased Test_Camera_Type;
      Camera_ID      : constant Camera_Index_Type := 1;


   begin
      Log_In (Debug);
      Set_Camera (Camera_ID, Camera'unchecked_access);
      Assert (Is_Valid (Camera_ID), "camera " & Camera_ID'img & " is invalid");
      Assert (Has_Camera (Camera_ID), "camera " & Camera_ID'img & " not set");
      declare
         Camera_Pointer    : constant Camera_Class_Access := Get_Camera (Camera_ID);

      begin
         Assert (Camera_Pointer /= Null, "no pointer for camera " & Camera_ID'img);
      end;

      Log_Out (Debug);
   end Test_Has_Camera;

   ----------------------------------------------------------------------------
   procedure URL_Open (
      Camera                     :    out Test_Camera_Type;
      URL                        : in     String;
      Port                       : in     Port_Type) is
   ----------------------------------------------------------------------------

   begin
      null;
   end URL_Open;

begin
--Debug := True;
--Trace_Options := True;
   Log_Here (Debug, "elaborate");

end Video.Lib.Unit_Test;

