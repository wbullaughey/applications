with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with Ada_Lib.Options_Interface;
with ADA_LIB.OS;
with Ada_Lib.Runstring_Options;
with Ada_Lib.Socket_IO;
with Ada_Lib.Strings;
with ADA_LIB.Trace; use Ada_Lib.Trace;
with Camera.Commands;
--with Configuration.Camera.State;
with Configuration.Camera;
with Configuration.State;
with Emulator;
with Main;
--with Camera.Commands.PTZ_Optics;
with Widgets.Adjust;
with Widgets.Control;
with Widgets.Configured;
with Widgets.Generic_Table;
with Widgets.Video;
with Windows.Top;

package body Camera.Lib is

   use type Configuration.State.Location_Type;
-- use type Ada_Lib.Options_Interface.Interface_Options_Constant_Class_Access;

   Trace_Option                  : constant Character := 't';
   Options_With_Parameters       : aliased constant
                                    Ada_Lib.Options_Interface.Options_Type :=
                                       Ada_Lib.Options_Interface.Create_Options (
                                          "cp" & Trace_Option);
   Options_Without_Parameters    : aliased constant
                                    Ada_Lib.Options_Interface.Options_Type :=
                                       Ada_Lib.Options_Interface.Create_Options (
                                          "Elr");
   Recursed                      : Boolean := False;

   -------------------------------------------------------------------------
   function Camera_Options
   return Options_Constant_Class_Access is
   -------------------------------------------------------------------------

   begin
      return Options_Constant_Class_Access (
         Ada_Lib.Options_Interface.Read_Only_Options);
   end Camera_Options;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
      Options               : in out Options_Type;
      From                  : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Recursed, Debug_Options or Trace_Options,
         "With Parameters " & Ada_Lib.Options_Interface.Image (
            Options_With_Parameters, False) &
         " Without Parameters " & Ada_Lib.Options_Interface.Image (
            Options_Without_Parameters, False) & " from " & From);

      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.With_Parameters,
         Options_With_Parameters);
      Ada_Lib.Runstring_Options.Options.Register (
         Ada_Lib.Runstring_Options.Without_Parameters,
         Options_Without_Parameters);

      return Log_Out_Checked (Recursed,
         Video.Lib.Options_Type (Options).Initialize,
         Debug_Options or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Initialize (
      Iterator                   :    out Source_Iterator_Type;
      Window                     : in     Gnoga.Gui.Base.Pointer_To_Base_Class;
      Source                     : in     String;
      Include_Options            : in     Boolean;
      Include_Non_Options        : in     Boolean;
      Argument_Seperator         : in     Character := ' ';
      Option_Prefix              : in     Character := '-';
      Skip                       : in     Natural := 0) is
   ----------------------------------------------------------------------------

      Modifiers                  : constant String := "";

   begin
      Log_In (Debug_Options or Trace_Options);
      Ada_Lib.Command_Line_Iterator.Internal.Iterator_Type (
         Iterator).Initialize (Source, Include_Options, Include_Non_Options,
         Argument_Seperator, Option_Prefix, Modifiers, Skip);
      Iterator.Window := Window;

      declare
         Iterator                : Ada_Lib.Command_Line_Iterator.Run_String.
                                    Runstring_Iterator_Type;

      begin
         Log_Here (Debug_Options or Trace_Options);
         Iterator.Initialize (Include_Options, Include_Non_Options,
            Modifiers   => Ada_Lib.Help.Modifiers);
--       Protected_Options.Process (Iterator);

      exception
         when Fault: Ada_Lib.Options.Failed =>
            Trace_Exception (Debug_Options or Trace_Options, Fault);
            Ada_Lib.Options.Program_Options.Help (Ada.Exceptions.Exception_Message (Fault), True);

         when Fault: others =>
            Trace_Exception (Debug_Options or Trace_Options, Fault);
            Ada_Lib.Options.Program_Options.Help (Ada.Exceptions.Exception_Message (Fault), True);
      end;

      Log_Out (Debug_Options or Trace_Options);

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
--    if Ada_Lib.Options_Interface.Read_Only_Options = Null then
--       raise Failed with "Read_Only_Options not set called from " & From;
--    end if;
--
--    Log_Here (Debug, "from " & From &
--       " Read_Only_Options tag " & Tag_Name (
--          Ada_Lib.Options_Interface.Read_Only_Options.all'tag));
--
--    return Options_Constant_Class_Access (
--       Ada_Lib.Options_Interface.Read_Only_Options);
-- end Options;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Options_Type;
      Iterator                   : in out ADA_LIB.Command_Line_Iterator.Abstract_Package.Abstract_Iterator_Type'class;
      Option                     : in     Ada_Lib.Options_Interface.
                                             Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug_Options, Option.Image);

      if Ada_Lib.Options_Interface.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is

            when 'c' =>
               Options.Directory.Construct (Iterator.Get_Parameter);

            when 'l' =>
               if Options.Location = Configuration.State.Remote then
                  Options.Bad_Option ("Remote option (r) and Local remote (l) are incompatable");
               end if;
               Options.Location := Configuration.State.Local;

            when 'r' =>    -- remote camera
               if Options.Simulate then
                  Options.Bad_Option ("Remote option (r) and Simulate (E) are incompatable");
               end if;
               Options.Location := Configuration.State.Remote;
--log_here ("remote " & Image (Options.Remote'address));

            when 'E' =>    -- simulate Standard.Camera
               if Options.Location = Configuration.State.Remote then
                  Options.Bad_Option ("Remote option (r) and Simulate (E) are incompatable");
               end if;
               Options.Simulate := True;

            when Trace_Option =>
               Options.Trace_Parse (Iterator);

            when others =>
               raise Failed with "Has_Option incorrectly passed" & Option.Image;

         end case;

         return Log_Out (True, Debug_Options or Trace_Options,
            Option.Image & " handled");
      else
         return Log_Out (
            Video.Lib.Options_Type (Options).Process_Option (Iterator, Option),
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
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Camera Lib";

   begin
      Log_In (Debug_Options or Trace_Options, "help mode " & Help_Mode'img);

      case Help_Mode is

      when Ada_Lib.Options.Program =>
         Log_Here (Debug_Options or Trace_Options, Quote ("Component", Component));

         Ada_Lib.Help.Add_Option ('c', "directory", "current directory", Component);
         Ada_Lib.Help.Add_Option ('l', "", "local camera", Component);
         Ada_Lib.Help.Add_Option ('r', "", "remote camera", Component);
         Ada_Lib.Help.Add_Option ('E', "", "simulate camera", Component);
         Ada_Lib.Help.Add_Option (Trace_Option, "trace options", "trace options", Component);
--       Ada_Lib.Help.Add_Option ('u', "camera URL", "URL", Component);
         New_Line;

      when Ada_Lib.Options.Traces =>
         New_Line;

         Put_Line (Component & " trace options (-" & Trace_Option & ")");
         Put_Line ("      a               all");
         Put_Line ("      c               camera configuration");
         Put_Line ("      C               camera commands");
         Put_Line ("      g               Widgets.Generic_Table");
         Put_Line ("      l               camera Library");
         Put_Line ("      L               camera library options");
         Put_Line ("      m               Main Window");
         Put_Line ("      s               Trace simulator");
         Put_Line ("      S               configuration state");
         Put_Line ("      T               Windows.Top");
         Put_Line ("      v               Trace Video communications");
         Put_Line ("      V               Trace Video widgets");
         Put_Line ("      wa              Adjust Window");
         Put_Line ("      wc              Control Window");
         Put_Line ("      wC              Configured Window");

      end case;

      Video.Lib.Options_Type (Options).Program_Help (Help_Mode);
      Log_Out (Debug_Options or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options                    : in out Options_Type;
      Iterator                   : in out Ada_Lib.Command_Line_Iterator.
                                             Abstract_Package.Abstract_Iterator_Type'class) is
   ----------------------------------------------------------------------------

      type Suboption_Type        is (Plain, Widget);

      Parameter                  : constant String := Iterator.Get_Parameter;
      Suboption                  : Suboption_Type := Plain;

   begin
      Log (Trace_Options or Debug_Options, Here, Who & Quote (" Parameter", Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug_Options, Quote ("Trace", Trace));

         case Suboption is
            when Plain =>

               case Trace is

                  when 'a' =>
                     Camera.Commands.Debug := True;
                     Configuration.Camera.Debug := True;
                     Configuration.State.Debug := True;
                     Debug_Options := True;
                     Debug := True;
                     Emulator.Debug := True;
                     Main.Debug := True;
                     Options.Lib_Debug := True;
                     Widgets.Adjust.Debug := True;
                     Widgets.Control.Debug := True;
                     Widgets.Configured.Debug := True;
                     Widgets.Generic_Table.Debug := True;

                  when 'c' =>
                     Configuration.Camera.Debug := True;

                  when 'C' =>
                     Camera.Commands.Debug := True;

                  when 'g' =>
                     Widgets.Generic_Table.Debug := True;

                  when 'l' =>
                     Debug := True;

                  when 'L' =>
                     Debug_Options := True;

                  when 'm' =>
                     Main.Debug := True;

                  when 's' =>
                     Emulator.Debug := True;

                  when 'S' =>
                     Configuration.State.Debug := True;

                  when 'T' =>
                     Windows.Top.Debug := True;

--                when 'u' =>    -- url for camera
--                   Options.Camera_URL.Construct (Iterator.Get_Parameter);

                  when 'V' =>
                     Widgets.Video.Debug := True;

                  when 'w' =>
                     Suboption := Widget;

                  when others =>
                     Options.Bad_Option (Quote (
                        "unexpected trace option", Trace) &
                        " for '" & Trace_Option & "'");

               end case;

         when Widget =>
               case Trace is

                  when 'a' =>
                     Widgets.Adjust.Debug := True;

                  when 'c' =>
                     Widgets.Control.Debug := True;

                  when 'C' =>
                     Widgets.Configured.Debug := True;

                  when others =>
                     Options.Bad_Option (Quote (
                        "unexpected trace option", Trace) &
                        " suboption " & Suboption'img &
                        " for '" & Trace_Option & "'");

               end case;
               Suboption := Plain;
         end case;
      end loop;
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
