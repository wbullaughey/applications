--with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options;
with ADA_LIB.OS;
--with Ada_Lib.Options.Create;
--with Ada_Lib.Options.Nested;
with Ada_Lib.Options.Program;
with Ada_Lib.Options.Runstring;
--with Ada_Lib.Options.Verification;
with Ada_Lib.Socket_IO;
--with Ada_Lib.Strings;
with ADA_LIB.String_Quote; use ADA_LIB.String_Quote;
with ADA_LIB.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
--with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Base;
--with Camera.Commands;
with Camera.Lib.Base;
with Camera.Lib.Options;
--with Camera.Configurations;
with Configuration.Camera;
--with Configuration.Camera.Setup;
--with Configuration.Camera.State;
--with Configuration.State;
--with Emulator;
--with Camera.Main;
with Camera.Options;
with Widgets.Adjust;
with Widgets.Control;
with Widgets.Configured;
with Widgets.Generic_Table;
with Widgets.Video;
--with Windows.Top;
--with Video.Lib;

package body Camera.Lib is

-- use type Configuration.State.Location_Type;
-- use type Ada_Lib.Options.Flag_List_Type;
-- use type Ada_Lib.Options.Interface_Options_Constant_Class_Access;

   Debug                   : Boolean renames Options.Camera_Options.
                                       Library_Debug;
   Debug_Options           : Boolean renames Options.Camera_Options.
                                       Options_Debug;
-- Library_Options         : Camera_Lib_Nested_Options_Class_Access := Null;
   Trace_Option            : constant Character := '2';
   Trace_Prefix            : constant Character :=
                                       Ada_Lib.Help.Trace_Modifier;
   Options_With_Parameters : aliased constant
                              Ada_Lib.Options.Flag_List_Type :=
                                 Ada_Lib.Options.Initialize (
                                    Trace_Option, Ada_Lib.Options.
                                       Unmodified_Flag);
-- Options_Without_Parameters    : aliased constant
--                                  Ada_Lib.Options.Flag_List_Type :=
--                                     Ada_Lib.Options.Create_Options (
--                                        Trace_Option,  -- local is default
--                                        Ada_Lib.Options.Unmodified_Flag) &
--                                     Ada_Lib.Options.Create_Options (
--                                        "", Ada_Lib.Options.Unmodified_Flag);
   Recursed                      : Boolean := False;

   -------------------------------------------------------------------------
   function Get_Camera_Modifiable_Nested_Options (
      From                       : in  String := Options_Here
   ) return Camera_Lib_Nested_Options_Class_Access is
   -------------------------------------------------------------------------

      Options  : constant Ada_Lib.Options.Program.
                  Nested_Program_Options_Class_Access :=
                     Ada_Lib.Options.Program.
                        Get_Modifiable_Nested_Program_Options;
   begin
      Log_Here (Debug or else Trace_Conversions, "from " & From);
      Tag_History (Debug, "options",Options.all'tag);
not_implemented;
return null;
--    return Library_Options;
   end Get_Camera_Modifiable_Nested_Options;

   -------------------------------------------------------------------------
   function Get_Camera_Readonly_Nested_Options (
      From                       : in  String := Options_Here
   ) return Camera_Lib_Nested_Options_Constant_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_Here (Debug or else Trace_Conversions, "from " & From);

      return Camera_Lib_Nested_Options_Constant_Class_Access (
         Ada_Lib.Options.Verification.Get_Ada_Lib_Read_Only_Nested_Options);
   end Get_Camera_Readonly_Nested_Options;

   -------------------------------------------------------------------------
   function Have_Options
   return Boolean is
   -------------------------------------------------------------------------

      Result   : constant Boolean :=
                  Ada_Lib.Options.Verification.Have_Ada_Lib_Verification_Options;
   begin
      return Log_Here (Result,
         Debug or else Trace_Pre_Post_Conditions or else not Result);
   end Have_Options;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
      Options               : in out Camera_Lib_Nested_Options_Type;
      From                  : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Recursed, Debug_Options or Trace_Options,
         Tag_Name ("options",
            Camera_Lib_Nested_Options_Type'class (Options)'tag) &
         " With Parameters " & Options_With_Parameters.Image);
--       " Without Parameters " & Ada_Lib.Options.Image (
--          Options_Without_Parameters, False) & " from " & From);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.With_Parameters,
         Options_With_Parameters);
--    Ada_Lib.Options.Runstring.Options.Register (
--       Ada_Lib.Options.Runstring.Without_Parameters,
--       Options_Without_Parameters);

      return Log_Out_Checked (Recursed,
         Video.Lib.Video_Lib_Nested_Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Initialize (
      Iterator             :    out Source_Iterator_Type;
      Window               : in     Gnoga.Gui.Base.Pointer_To_Base_Class;
      Source               : in     String;
      Include_Options      : in     Boolean;
      Include_Non_Options  : in     Boolean;
      Argument_Seperator   : in     Character := ' ';
      Option_Prefix        : in     Character := '-';
      Skip                 : in     Natural := 0) is
   ----------------------------------------------------------------------------

      Log_It      : constant Boolean := Debug or else
                                        Debug_Options or else
                                        Trace_Options;
      Modifiers                  : constant String := "";

   begin
      Log_In (Log_It);
      Ada_Lib.Command_Line_Iterator.Internal.Iterator_Type (
         Iterator).Initialize (Source, Include_Options, Include_Non_Options,
         Argument_Seperator, Option_Prefix, Modifiers, Skip);
      Iterator.Window := Window;

      declare
         Iterator                : Ada_Lib.Command_Line_Iterator.Run_String.
                                    Runstring_Iterator_Type;

      begin
         Log_Here (Log_It);
         Iterator.Initialize (Include_Options, Include_Non_Options,
            Modifiers   => Ada_Lib.Help.Modifiers);
--       Protected_Options.Process (Iterator);

      exception
         when Fault: Ada_Lib.Options.Failed =>
            Trace_Exception (Log_It, Fault);
--          Ada_Lib.Options.Flags.Display_Help (
--             Ada.Exceptions.Exception_Message (Fault), True);
            raise;

         when Fault: others =>
            Trace_Exception (Log_It, Fault);
--          Ada_Lib.Options.Flags.Display_Help (Ada.Exceptions.Exception_Message (Fault), True);
            raise;
      end;

      Log_Out (Log_It);

   end Initialize;

   ----------------------------------------------------------------------------
   overriding
   procedure Open (
      Camera                     :    out General_Camera_Type;
      Camera_Address             : in     Address_Type;
      Port_Number                : in     Port_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "Address_Kind " & Camera_Address.Address_Kind'img);
      case Camera_Address.Address_Kind is

         when Ada_Lib.Socket_IO.IP =>
            General_Camera_Type'class (Camera).IP_Open (
               Camera_Address.IP_Address, Port_Number);

         when Ada_Lib.Socket_IO.Not_Set =>
            raise Failed with "socket address not set from " & Here;

         when Ada_Lib.Socket_IO.URL =>
            General_Camera_Type'class (Camera).Host_Open (
               Camera_Address.URL_Address.Coerce, Port_Number);

      end case;
      Log_Out (Debug);
   end Open;

-- ----------------------------------------------------------------------------
-- function Options (
--    From                       : in     String :=
--                                           Standard.GNAT.Source_Info.Source_Location
-- ) return Options_Constant_Class_Access is
-- ----------------------------------------------------------------------------
--
-- begin
--    if Ada_Lib.Options.Read_Only_Options = Null then
--       raise Failed with "Read_Only_Options not set called from " & From;
--    end if;
--
--    Log_Here (Debug, "from " & From &
--       Tag_Name (" Read_Only_Options",
--          Ada_Lib.Options.Read_Only_Options.all'tag));
--
--    return Options_Constant_Class_Access (
--       Ada_Lib.Options.Read_Only_Options);
-- end Options;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options  : in out Camera_Lib_Nested_Options_Type;
      Iterator : in out Ada_Lib.Options.
                           Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image &
         " help test " & Ada_Lib.Options.Ada_Lib_Environment.Help_Test'img);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Ada_Lib.Options.Null_Flag_List) then
         case Option.Option is

            when Trace_Option =>
               Options.Trace_Parse (Iterator);

            when others =>
               declare
                  Message  : constant String :=
                              "Has_Option incorrectly passed " &
                              Option.Image;
               begin
                  Log_Exception (Trace_Options or Debug_Options, Message);
                  raise Failed with Message;
               end;
         end case;

         return Log_Out (True, Debug_Options or Trace_Options,
            Option.Image & " handled");
      else
         return Log_Out (
               Video.Lib.Video_Lib_Nested_Options_Type (Options).Process_Option (
                  Iterator, Option),
               Trace_Options or Debug_Options, "other " & Option.Image);
      end if;

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Camera_Lib_Nested_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component   : constant String := "Camera Lib";
      Log_It      : constant Boolean := Debug_Options or Trace_Options;

   begin
      Log_In (Log_It, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program_Mode =>
         Log_Here (Log_It,
            Quote ("Component", Component));

         Ada_Lib.Help.Create_Option (Trace_Option, True, "trace lib options",
            "Camera Lib Debug", Component, Ada_Lib.Options.Unmodified_Flag);
--       Ada_Lib.Help.Create_Option ('u', "camera URL", "URL", Component, Ada_Lib.Options.Unmodified_Flag);
         New_Line;

      when Ada_Lib.Options.Trace_Mode =>
         Ada_Lib.Help.Set_Has_Trace (Trace_Option, Ada_Lib.Options.Unmodified_Flag);
         New_Line;

         Put_Line (Component & " trace options (-" &
            Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      b               Camera.Base.debug");
         Put_Line ("      B               Camera.Lib.Base.debug");
--       Put_Line ("      c               camera configuration");
         Put_Line ("      C               " &
            "Camera.Commands.Debug: Camera_Options.Commands_Debug");
         Put_Line ("      d               camera Debug");
         Put_Line ("      g               Widgets.Generic_Table");
         Put_Line ("      l               camera Library");
         Put_Line ("      L               camera library options");
         Put_Line ("      m               Camera.Main.Debug");
         Put_Line ("      M               Camera_Control.Debug");
         Put_Line ("      s               Camera.Configuration.Debug");
         Put_Line ("      S               Camera.Configurations.Debug");
--       Put_Line ("      v               Trace Video communications");
         Put_Line ("      V               Trace Video widgets");
         Put_Line ("      " & Trace_Prefix &
                          "a              Adjust Window");
         Put_Line ("      " & Trace_Prefix &
                          "c              Widgets.Control debyg");
         Put_Line ("      " & Trace_Prefix &
                          "C              Widgets.Configured debug");
         Put_Line ("      " & Trace_Prefix &
                          "l              List camera commands");
         Put_Line ("      " & Trace_Prefix &
                          "p              Configuration.Camera.Setup.Debug");
--       Put_Line ("      " & Trace_Prefix &
--                        "s              Configuration");
         Put_Line ("      " & Trace_Prefix &
                          "S              Configuration.Camera.State.Debug");

      end case;

      Video.Lib.Video_Lib_Nested_Options_Type (Options).Program_Help (
         Help_Mode);
      Log_Out (Log_It);
   end Program_Help;

-- ----------------------------------------------------------------------------
-- procedure Set_Library_Options (
--    Library_Options_Pointer    : in        Camera_Lib_Nested_Options_Class_Access;
--    From                       : in        String := Here) is
-- ----------------------------------------------------------------------------
--
-- begin
--    Library_Options := Library_Options_Pointer;
-- end Set_Library_Options;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Camera_Lib_Nested_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

      Parameter                  : constant String := Iterator.Get_Parameter;
      Suboption                  : Ada_Lib.Options.Flag_Option_Kind_Type :=
                                    Ada_Lib.Options.Plain;

   begin
      Log_In (Trace_Options or Debug_Options,
         Quote (" Parameter", Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug_Options,
            "suboptions " & Suboption'img & Quote (" Trace", Trace));

         case Suboption is
            when Ada_Lib.Options.Plain =>

               case Trace is

                  when 'a' =>
                     Base.List_Commands := True;
                     Lib.Options.Camera_Options.Base_Debug := True;
                     Lib.Options.Camera_Options.Base_Lib_Debug := True;
                     Lib.Options.Camera_Options.Camera_Debug := True;
                     Lib.Options.Camera_Options.Commands_Debug := True;
                     Lib.Options.Camera_Options.State_Debug := True;
                     Lib.Options.Camera_Options.States_Debug := True;
                     Lib.Options.Configuration_Options.State_Debug := True;
                     Configuration.Debug := True;
                     Debug_Options := True;
                     Debug := True;
--                   Emulator.Debug := True;
--                   Options.Lib_Debug := True;
                     Widgets.Adjust.Debug := True;
                     Widgets.Control.Debug := True;
                     Widgets.Configured.Debug := True;
                     Widgets.Generic_Table.Debug := True;

                  when 'b' =>
                     Lib.Options.Camera_Options.Base_Debug := True;

                  when 'B' =>
                     Lib.Options.Camera_Options.Base_Lib_Debug := True;

                  when 'C' =>
                     Lib.Options.Camera_Options.Commands_Debug := True;

                  when 'd' =>
                     Lib.Options.Camera_Options.Camera_Debug := True;

                  when 'g' =>
                     Widgets.Generic_Table.Debug := True;

                  when 'l' =>
                     Debug := True;

                  when 'L' =>
                     Debug_Options := True;

                  when 'm' =>
                     Lib.Options.Camera_Options.Main_Debug := True;

                  when 'M' =>
                     Lib.Options.Camera_Options.Camera_Control_Debug := True;

                  when 's' =>
                     Lib.Options.Camera_Options.State_Debug := True;
--                   Emulator.Debug := True;

                  when 'S' =>
                     Lib.Options.Camera_Options.States_Debug := True;

--                when 'u' =>    -- url for camera
--                   Lib.Options.Camera_URL.Construct (Iterator.Get_Parameter);

                  when 'V' =>
                     Widgets.Video.Debug := True;

                  when Trace_Prefix =>
                     Suboption := Ada_Lib.Options.Modified;

                  when others =>
                     Options.Bad_Option (Quote (
                        "unexpected trace option", Trace) &
                        " for '" & Trace_Option & "'");

               end case;

         when Ada_Lib.Options.Modified =>
               case Trace is

                  when 'a' =>
                     Widgets.Adjust.Debug := True;


                  when 'c' =>
                     Widgets.Control.Debug := True;

                  when 'C' =>
                     Widgets.Configured.Debug := True;

                  when 'l' =>
                     Base.List_Commands := True;

                  when 'p' =>
                     Lib.Options.Configuration_Options.Setup_Debug := True;

--                when 's' =>
--                   Lib.Options.Camera_Options.Camera_Debug := True;

                  when 'S' =>
                     Lib.Options.Configuration_Options.State_Debug := True;

                  when others =>
                     Options.Bad_Option (Quote (
                        "unexpected trace option",
                        Trace) &
                     " suboption " & Suboption'img &
                     " for '" & Trace_Option & "'");

               end case;
               Suboption := Ada_Lib.Options.Plain;

            when Ada_Lib.Options.Nil_Option =>
               not_implemented;

         end case;
      end loop;
      Log_Out (Trace_Options or Debug_Options);
   end Trace_Parse;

begin
--Elaborate := True;
--Trace_Options := True;
--Debug := True;
--Debug_Options := True;
   Log_Here (Debug or Debug_Options or Elaborate or Trace_Options);

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (Ada_Lib.OS.No_Error);

end Camera.Lib;
