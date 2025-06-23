with Ada.Containers.Indefinite_Vectors;
--with Ada.Directories;
--with Ada.Strings.Fixed;
with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Command_Line_Iterator;
with Ada_Lib.Help;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.OS.Run;
with Ada_Lib.Parser;
with Ada_Lib.Options.Runstring;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Command_Name;

package body Driver is

   use Ada_Lib.Strings.Unlimited;
   use type Ada_Lib.Options.Options_Type;
   use type Ada_Lib.OS.OS_Exit_Code_Type;

-- subtype String_Type           is String_Type;
--
-- use type String_Type;

   type Element_Type             is record
      Routine                    : String_Type;
      Suite                      : String_Type;
   end record;

   type Parameter_Type           is record
      With_Parameters            : Ada_Lib.Options.Options_Access;
      Without_Parameters         : Ada_Lib.Options.Options_Access;
   end record;

   type Parameters_Type          is array (Boolean) of Parameter_Type;

   function Compare_Entry (
      Left, Right                : in     Element_Type
   ) return Boolean;

   package Queue_Package   is new Ada.Containers.Indefinite_Vectors (
      Index_Type           => Positive,
      Element_Type         => Element_Type,
      "="                  => Compare_Entry);

   subtype Queue_Type            is Queue_Package.Vector;

   procedure Process_Line (
      Line                       : in     String);

   Debug                         : Boolean := False;
   Debug_Options                 : Boolean := False;
   Driver_Directory              : constant Character := 'X';
   Directory_Option              : constant Character := 'D';
   Driver_Test_Trace_Option      : constant Character := 'd';
   Driver_Trace_Option           : constant Character := 't';
   Parameters  : constant Parameters_Type := (
                  False    => (     -- driver
                     With_Parameters      =>
                        new Ada_Lib.Options.Options_Type'(
                           Ada_Lib.Options.Create_Options (Driver_Directory &
                              Directory_Option & "Ru", Option_Modifier) &
                           Ada_Lib.Options.Create_Options (
                              "s",Ada_Lib.Options.Unmodified) &
                           Ada_Lib.Options.Create_Options (Driver_Trace_Option &
                              "op", Ada_Lib.Options.Unmodified)
                        ),
                     Without_Parameters   =>
                        new Ada_Lib.Options.Options_Type'(
                           Ada_Lib.Options.Create_Options ("l", Option_Modifier)
                        )
                  ),
                  True    => (      -- unit test
                     With_Parameters      =>
                        new Ada_Lib.Options.Options_Type'(
                           Ada_Lib.Options.Create_Options (
                              Driver_Test_Trace_Option,
                              Ada_Lib.Options.Unmodified) &
                           Ada_Lib.Options.Create_Options (Driver_Directory &
                              Directory_Option & "u", Option_Modifier)
                        ),
                     Without_Parameters   =>
                        new Ada_Lib.Options.Options_Type'(
                           Ada_Lib.Options.Create_Options ("l", Option_Modifier)
                        )
                  )
               );
   Protected_Options             : Driver_Options_Class_Access := Null;
   Queue                         : Queue_Type;

   ----------------------------------------------------------------
   function Compare_Entry (
      Left, Right                : in     Element_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end Compare_Entry;

   ---------------------------------------------------------------
   procedure Execute (
      Parameters                 : in     String;
      Process_Line               : access procedure (
         Line                    : in     String)) is
   ---------------------------------------------------------------

--    use Ada.Text_IO;
--    use Ada.Strings.Fixed;

      Camera_Directory           : constant String :=
                                    Protected_Options.Camera_Directory.Coerce;
      Camera_Program             : constant String :=
                                    Camera_Directory & "/bin/camera_aunit";
      Scratch_File               : Ada_Lib.OS.File_Descriptor;
      Scratch_Name               : Ada_Lib.OS.Temporary_File_Name;

      ------------------------------------------------------
      function Error_Message (
         Result                  : in     Ada_Lib.OS.OS_Exit_Code_Type
      ) return String is
      ------------------------------------------------------

      begin
         return Quote ("could not run camera controller ",
            Camera_Program) & Quote (" parameters", Parameters &
            " result" & Result'img);
      end Error_Message;
      ------------------------------------------------------

   begin
      Log_In (Debug, Quote (" Camera_Directory", Camera_Directory) &
         Quote (" Camera_Program", Camera_Program) &
         Quote (" Parameters", Parameters));

      if Process_Line = Null then
         declare
            Result               : constant Ada_Lib.OS.OS_Exit_Code_Type :=
                                    Ada_Lib.OS.Run.Spawn (
                                       Camera_Program, Parameters);
         begin
            Log_Here (Debug, "result " & Result'img);
            if Result /= Ada_Lib.OS.No_Error then
               raise Failed with Error_Message (Result);
            end if;
            Log_Here (Debug, "returned" & Result'img & " from spawn");
         end;
      else
         Ada_Lib.OS.Create_Scratch_File (Scratch_File, Scratch_Name);
         Ada_Lib.OS.Close_File (Scratch_File);


         Log_Here (Debug, Quote ("scratch name", Scratch_Name));
         declare
            Result               : constant Ada_Lib.OS.OS_Exit_Code_Type :=
                                    Ada_Lib.OS.Run.Spawn (Camera_Program,
                                       Parameters, Scratch_Name);
         begin
            Log_Here (Debug, "result " & Result'img & " returned from spawn");

            declare
               Output_File             : File_Type;
   --          Process_Output          : Boolean := True;

            begin
               Open (Output_File, In_File, Scratch_Name);
               Log_Here (Debug, "output opened");

               while not End_Of_File (Output_File) loop
                  declare
                     Line              : constant String := Get_Line (Output_File);

                  begin
                     Log_Here (Debug, Quote ("line", Line));

                     Process_Line (Line);
   --                if Index (Line, "exception trace") > 0 then
   --                   exit;
   --                end if;
                  end;
               end loop;
               Close (Output_File);
            end;
            Put_Line ("camera test result " & Result'img &
               " returned from spawn");
         end;

      end if;


      Log_Out (Debug);

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Execute;

   ----------------------------------------------------------------
   function Get_Modifiable_Options (
      From                       : in  String := Ada_Lib.Trace.Here
   ) return Driver_Options_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug_Options or Trace_Options, "from " & From);
      return Driver_Options_Class_Access (
         Ada_Lib.Options.Actual.Get_Ada_Lib_Modifiable_Nested_Options);
   end Get_Modifiable_Options;

   ---------------------------------------------------------------

   procedure Get_Tests is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Execute ("-@d", Process_Line'access);
      Log_Out (Debug);
   end Get_Tests;

   ---------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Driver_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ---------------------------------------------------------------

      Selected_Parameters        : Parameter_Type renames
                                    Parameters (Options.Testing);
   begin
      Log_In (Debug_Options or Trace_Options, "testing " & Options.Testing'img &
         " with parameters " & Ada_Lib.Options.Image (
            Selected_Parameters.With_Parameters.all) &
         " without parameters " & Ada_Lib.Options.Image (
            Selected_Parameters.Without_Parameters.all) &
         " Initialized " & Options.Initialized'img &
         " from " & From);
      Protected_Options := Options'unchecked_access;
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
            Selected_Parameters.With_Parameters.all);
      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
            Selected_Parameters.Without_Parameters.all);

      return Log_Out (
         Ada_Lib.Options.Actual.Nested_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);

   end Initialize;

   ---------------------------------------------------------------
   overriding
   function Initialize (
     Options                     : in out Program_Options_Type;
     From                        : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options, "from " & From);
      return Log_Out (Options.Driver_Options.Initialize and then
         Ada_Lib.Options.Actual.Program_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

   ---------------------------------------------------------------
   procedure Process_Line (
      Line                       : in     String) is
   ---------------------------------------------------------------

      use Ada_Lib.Parser;

      Parser                     : Iterator_Type := Initialize (
                                    Seperators  => ": ",
                                    Value       => Line);
   begin
      Log_In (Debug, (if Protected_Options.List_Output then "listing"
                      else Quote ("line", Line)));

      if Protected_Options.List_Output then
         Put_Line ("Camera:" & Line);
      end if;

      if    Line'length > 6 and then
            Line (Line'first .. Line'first + 5) = "suite:" then
                                       -- output of list suites
         declare
            Suite                   : constant String :=
                                       Parser.Get_Value (Do_Next => True);
            Suite_Name              : constant String :=
                                       Parser.Get_Value (Do_Next => True);
            Routine                 : constant String :=
                                       Parser.Get_Value (Do_Next => True);
            Routine_Name            : constant String :=
                                       Parser.Get_Value;
         begin
            Log_Here (Debug,
               Quote ("suite", Suite) &
               Quote (" suite name", Suite_Name) &
               Quote (" routine", Routine) &
               Quote (" routine name", Routine_Name));

            if    Suite = "suite" and then
                  Routine = "routine" and then
                  Routine_Name (Routine_Name'last) /= '*' then
               Push (Suite_Name, Routine_Name);
            end if;
         end;
      end if;
      Log_Out (Debug);

   exception
      when Fault: Ada_Lib.Parser.Underflow =>
         Trace_Exception (Debug, Fault,
            "missing suite or routine from test list");
         Put_Line (Quote ("missing suite or routine in test list line", Line));
         Ada_Lib.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

      when Fault: others =>
         Trace_Exception (Debug, Fault, "could not parse test list");
         raise;

   end Process_Line;

   ---------------------------------------------------------------
   overriding
   function Process_Option (  -- process one option
     Options                     : in out Driver_Options_Type;
      Iterator                   : in out Ada_Lib.Options.
                                          Command_Line_Iterator_Interface'class;
      Option                     : in     Ada_Lib.Options.
                                             Option_Type'class
   ) return Boolean is
   ---------------------------------------------------------------

      Selected_Parameters        : Parameter_Type renames
                                    Parameters (Options.Testing);
   begin
      Log_In (Debug_Options or Trace_Options, "testing " & Options.Testing'img &
         " " & Option.Image);

      if Ada_Lib.Options.Has_Option (Option,
         Selected_Parameters.With_Parameters.all,
         Selected_Parameters.Without_Parameters.all) then
         case Option.Kind is
            when Ada_Lib.Options.Plain =>
               case Option.Option is

                  when Directory_Option =>
                     Options.Camera_Directory.Construct (Iterator.Get_Parameter);

                  when 'l' =>
                     Options.List_Output := True;

                  when 'r' =>
                     Options.Remote_Camera := True;

                  when 's' =>
                     Options.Suite.Construct (Iterator.Get_Parameter);
                     Log_Here (Debug_Options, Quote ("suite", Options.Suite));

                  when  Driver_Trace_Option |
                        Driver_Test_Trace_Option =>
                    declare
                       Parameter
                                : constant String := Iterator.Get_Parameter;
                    begin
                       Log (Debug_Options or Trace_Options, Here, " process parameter  " & Quote (Parameter));

                       for Trace of Parameter loop
                          Log_Here (Debug_Options or Trace_Options, Quote ("Trace", Trace));
                          case Trace is

                             when 'a' =>
                                Debug_Options := True;
                                Protected_Options.Main_Debug := True;

                             when 'o' =>
                                Debug_Options := True;

                             when 'p' =>    -- program trace
                                Protected_Options.Main_Debug := True;

                             when 't' =>
                                Debug := True;

                             when others =>
                                Options.Bad_Option (Quote (
                                   "unexpected trace option", Trace));
                                return False;

                          end case;
                       end loop;
                    end;

                  when others =>
                     declare
                        Message  : constant String :=
                                    "Has_Option incorrectly passed " &
                                    Option.Image;
                     begin
                        Log_Exception (Debug_Options or Trace_Options, Message);
                        raise Failed with Message;
                     end;
               end case;

            when Ada_Lib.Options.Modified =>
               case Option.Option is

                  when Directory_Option =>
                     Options.Camera_Directory.Construct (Iterator.Get_Parameter);

                  when 'R' =>
                     Options.Routine.Construct (Iterator.Get_Parameter);

                  when 'u' =>
                     Options.Suite.Construct (Iterator.Get_Parameter);

                  when Driver_Directory =>    -- X
                     declare
                        Argument : constant String :=
                                       Iterator.Get_Argument;
                     begin
                        Log_Here (Debug_Options or Trace_Options,
                           Quote (" Argument", Argument));

                        Protected_Options.Camera_Options :=
                           Protected_Options.Camera_Options & Argument (
                              Argument'first + 2 .. Argument'last);

                        Log_Here (Debug_Options or Trace_Options, Quote ("options",
                           Protected_Options.Camera_Options));
                     end;

                  when others =>
                     declare
                        Message  : constant String :=
                                    "Has_Option incorrectly passed " &
                                    Option.Image;
                     begin
                        Log_Exception (Debug_Options or Trace_Options, Message);
                        raise Failed with Message;
                     end;
               end case;

            when Ada_Lib.Options.Nil_Option =>
               declare
                  Message  : constant String := "nil option";

               begin
                  Log_Exception (Debug_Options or Trace_Options, Message);
                  raise Failed with Message;
               end;

         end case;

         return Log_Out (True, Debug_Options or Trace_Options, Option.Image &
            " handled");
      else
         return Log_Out (Ada_Lib.Options.Actual.Nested_Options_Type (
            Options).Process_Option (Iterator, Option), Debug or Trace_Options,
            "not handled");
      end if;

   exception
      when Fault: others =>
            Trace_Exception (Debug, Fault);
            Ada_Lib.OS.Exception_Halt (Fault);
            return False;

   end Process_Option;

   ---------------------------------------------------------------
   overriding
   function Process_Option (  -- process one option
      Options     : in out Program_Options_Type;
      Iterator    : in out Ada_Lib.Options.
                              Command_Line_Iterator_Interface'class;
      Option      : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean is
   ---------------------------------------------------------------

   begin
      Log_In (Debug_Options or Trace_Options);
      return Log_Out (
         Options.Driver_Options.Process_Option (Iterator, Option) or else
         Ada_Lib.Options.Actual.Program_Options_Type (Options).Process_Option (
            Iterator, Option),
         Debug_Options or Trace_Options);
   end Process_Option;

   ---------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Driver_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ---------------------------------------------------------------

      Component                  : constant String := Command_Name;

   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img &
         Quote (" component", Component));
      Ada_Lib.Options.Actual.Nested_Options_Type (Options).Program_Help (Help_Mode);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Ada_Lib.Help.Add_Option (Directory_Option, "subdirectory",
            "subdirectory to run camera app from", Component, Option_Modifier);
         Ada_Lib.Help.Add_Option ('l', "", "list output from camera app",
            Component);
--       Ada_Lib.Help.Add_Option ('r', "",
--          "remote camera", Component);
         Ada_Lib.Help.Add_Option ('R', "routine",
            "routine to run, multiple allowed", Component, Option_Modifier);

         Ada_Lib.Help.Add_Option ('u', "suite",
            "suite to run, multiple allowed", Component, Option_Modifier);

         Ada_Lib.Help.Add_Option (
            (if Options.Testing then
               Driver_Test_Trace_Option
            else
               Driver_Trace_Option
            ), "Trace Options", "driver trace options", Component);
         Ada_Lib.Help.Add_Option (Driver_Directory, "options", "options to pass",
            Component, Option_Modifier);

      when Ada_Lib.Options.Traces =>
         New_Line;
         Put_Line ("driver trace options (-" &
            (if Options.Testing then
               Driver_Test_Trace_Option
            else
               Driver_Trace_Option
            ));
         Put_Line ("      a               all");
         Put_Line ("      o               program options trace");
         Put_Line ("      p               program trace");
         Put_Line ("      t               driver trace");

      end case;
      Log_Out (Debug_Options or Trace_Options);

   end Program_Help;

   ---------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Program_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ---------------------------------------------------------------

      Component                  : constant String := Command_Name;

   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img &
         Quote (" component", Component));
      Ada_Lib.Options.Actual.Program_Options_Type (Options).Program_Help (Help_Mode);
      Options.Driver_Options.Program_Help (Help_Mode);
      Log_Out (Debug_Options or Trace_Options);

   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Push (
      Suite                      : in     String;
      Routine                    : in     String) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("suite", Suite) & Quote (" routine", Routine));

      Queue_Package.Append (
         Container   => Queue,
         New_Item    => Element_Type'(
            Routine     => Coerce (Routine),
            Suite       => Coerce (Suite)),
         Count       => 1);
      Log_Out (Debug);
   end Push;

   ----------------------------------------------------------------------------
   procedure Queue_Tests is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "testing " & Protected_Options.Testing'img &
         " Protected_Options tag " & Tag_Name (Protected_Options.all'tag));

--    Quote ("suite", Protected_Options.Suite) & Quote (" routine",
--       Protected_Options.Routine));

      if Protected_Options.Routine.Length = 0 then -- need list of suites and routines
         Get_Tests;
      else
         if Protected_Options.Suite.Length = 0 then
            raise Failed with "no suite";
         else
            Push (Protected_Options.Suite.Coerce, Protected_Options.Routine.Coerce);
         end if;
      end if;
      Log_Out (Debug);
   end Queue_Tests;

   ----------------------------------------------------------------------------
   procedure Run_Selection is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, Quote ("suite", Protected_Options.Suite) &
         Quote (" Camera_Options", Protected_Options.Camera_Options));

      for Element of Queue loop
         Log_Here (Debug, Quote ("suite", Element.Suite) &
             Quote (" routine", Element.Routine));
--       if Element.Suite.Length = 0 or else  -- test all suites
--             Suite = Element.Suite then
            declare
               Options  : constant String :=
                           "-" & Option_Modifier & Directory_Option & " " &
                           Protected_Options.Camera_Directory.Coerce &
                           (if Protected_Options.Remote_Camera then
                              " -r "
                           else
                              "") &
                           " -s " & Element.Suite.Coerce &
                           " -e " & Element.Routine.Coerce &
                           " " & Protected_Options.Camera_Options.Coerce;
            begin
               Log_Here (Debug, Quote ("run string", Options));

               Put_Line (Quote ("run suite", Element.Suite) &
                  Quote (" routine", Element.Routine) & " " & Here);

               Execute (Options, Process_Line'access);
            end;
--       end if;
      end loop;
      Log_Out (Debug);
   end Run_Selection;

   ----------------------------------------------------------------------------
   procedure Set_Protected_Options (
      Options                    : in Driver_Options_Class_Access) is
   ----------------------------------------------------------------------------

   begin
      Protected_Options := Options;
   end Set_Protected_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Driver_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Trace_Parse;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Program_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug);
      Log_Out (Debug);
   end Trace_Parse;

begin
--Debug_Options := True;
--Elaborate := True;
--Trace_Options := True;
   Include_Program := True;
   Include_Task := True;
   Log_Here (Debug_Options or Trace_Options or Elaborate);
end Driver;
