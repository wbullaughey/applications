with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Help;
--with ADA_LIB.Options.GNOGA.Database.AUnit;
--with Ada_Lib.Test;
with ADA_LIB.OS;
--with ADA_LIB.Template;
--with ADA_LIB.Text;
with ADA_LIB.Trace; use ADA_LIB.Trace;

package body Runtime_Options is

   procedure Program_Help;

   Options_With_Parameters       : aliased constant String := "mptuw";
   Options_Without_Parameters    : aliased constant String := "c";
   Protected_Options             : aliased Options_Type;
   Register_Options              : ADA_LIB.Command_Line_Iterator.Register_Options_Type (
                                    Options_With_Parameters'access, Options_Without_Parameters'access,
                                    new String'(Here));
   pragma Unreferenced (Register_Options);   -- declared for side affect of Initialization

   ----------------------------------------------------------------
   function Get_Options (
      From                       : in  String := ADA_LIB.Trace.Here
   ) return Options_Constant_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, Image (Options.all'address) & " from " & From &
         " verbose " & Options.Verbose'img);
      return Options;
   end Get_Options;

   -------------------------------------------------------------------------
   function Get_Modifiable_Options (
      From                       : in  String := ADA_LIB.Trace.Here
   ) return Options_Class_Access is
   -------------------------------------------------------------------------

   begin
      Log_In (Debug, "From " & From);
      return Protected_Options'access;
   end Get_Modifiable_Options;

   -------------------------------------------------------------------------
   procedure Initialize is
   -------------------------------------------------------------------------

   begin
      Log_In (debug);
      Options := Protected_Options'access;
      Process;
      if not Protected_Options.Set_Options then      -- set any local pointers, calls down derivation tree
         Put_Line ("Set_Options did not call to root");
         ADA_LIB.OS.Immediate_Halt (0);
      end if;
      Log_Out (debug);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Process is
   ----------------------------------------------------------------------------

     Iterator                   : ADA_LIB.Command_Line_Iterator.Run_String.Iterator_Type :=
                                   ADA_LIB.Command_Line_Iterator.Run_String.Initialize (
                                      Include_Options         => True,
                                      Include_Non_Options     => True);

   begin
      Log (Debug, Here, Who & " enter " & Quote ("Options_With_Parameters", Options_With_Parameters));
      Protected_Options.Process (Iterator);
      Log_Out (Debug);
   end Process;

   ----------------------------------------------------------------------------
   overriding
   procedure Process (
     Options                     : in out Options_Type;
     Iterator                    : in out ADA_LIB.Command_Line_Iterator.
                                    Abstract_Package.Iterator_Type'class) is
   ----------------------------------------------------------------------------

   begin
     Log_In (Debug);
     while not Iterator.At_End loop
        if Iterator.Is_Option then
           declare
              Option         : constant Character := Iterator.Get_Option;

           begin
               Log (Debug, Here, " option  " & Quote (Option));
               if Options.Process_Option (Iterator, Option) then
                  Log (Debug, Here, " option  " & Quote (Option));
               else
                  Log (Debug, Here, " option  " & Quote (Option) & " not processed");
                  ADA_LIB.Options.Bad_Option (Option, "run string");
                  Program_Help;
                  exit;
               end if;
           end;
        else
           raise ADA_LIB.Options.Help with "unexpected '" & Iterator.Get_Parameter & "' on run string";
        end if;

        if not Iterator.At_End then
           Iterator.Advance;
        end if;
     end loop;

     Log (Debug, Here, Who & " exit ");

   exception

      when Fault: ADA_LIB.Options.Help =>
         Trace_Exception (Debug, Fault);
         declare
            Message              : constant String := Ada.Exceptions.Exception_Message (Fault);

         begin
            if Message /= "Help" then
               Put_Line (Message);
            end if;

            Program_Help;
            Log_Here (Debug);
            ADA_LIB.OS.Immediate_Halt(0);
         end;

      when Fault: others =>
         Trace_Exception (True, Fault);
         raise;

   end Process;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options                    : in out Options_Type;
      Iterator                   : in out ADA_LIB.Command_Line_Iterator.Abstract_Package.Iterator_Type'class;
      Option                     : in     Character
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug, Quote ("Option", Option));

      case Option is

         when 'c' =>
            Options.Protocol := CURL;

         when 'm' =>
            Options.Message_Path.Construct (Iterator.Get_Parameter);

         when 'p' =>
            Options.Parameter_Path.Construct (Iterator.Get_Parameter);

         when 't' =>
            declare
               Parameter
                        : constant String := Iterator.Get_Parameter;
            begin
               for Trace of Parameter loop
                  Log (Trace_Options or Debug, Here, " enter " & Quote ("Trace", Trace));
                  case Trace is

                     when 'a' =>
                        Debug := True;
                        Options.Debug := True;

                     when 'r' =>
                        Debug := True;

                     when 't' =>
                        Options.Debug := True;

                     when others =>
                        Put_Line ("unexpected trace option '" & Trace & "'");
                        return False;

                  end case;
               end loop;
            end;

         when 'u' =>
            Options.User.Construct (Iterator.Get_Parameter);

         when 'w' =>
            Options.Password.Construct (Iterator.Get_Parameter);

         when others =>
            Log (Debug, Here, Who & " other option" & Quote (" Option", Option));
            return ADA_LIB.Options.Options_Type (
                     Options).Process_Option (Iterator, Option);

      end case;

      Log (Debug, Here, Who & " exit" & Quote (" option", Option) & " handled");
      return True;
   end Process_Option;

   ----------------------------------------------------------------------------
   procedure Program_Help is     -- common for all programs that use ADA_LIB.Options.GNOGA
   ----------------------------------------------------------------------------

      -------------------------------------------------------------------------
      procedure Print_Help (
         Line                    : in     String) is
      -------------------------------------------------------------------------

      begin
         Put ("    ");
         Put_Line (Line);
      end Print_Help;
      -------------------------------------------------------------------------

   begin
      Log_In (Debug);

      Protected_Options.Program_Help (ADA_LIB.Options.Program);
      New_Line;

      Ada_Lib.Help.Add_Option ('t', "trace options", Ada.Command_Line.Command_Name);

      Ada_Lib.Help.Display (Print_Help'access);
      New_Line;

      Protected_Options.Program_Help (ADA_LIB.Options.Traces);
      New_Line;

      Put_Line (Ada.Command_Line.Command_Name & " trace options (-t)");
      Put_Line ("      a               all");
      Put_Line ("      r               Runtime_Options");
      Put_Line ("      t               application trace>");

      Log_Out (Debug, "halting");
      ADA_LIB.OS.Immediate_Halt(0);
   end Program_Help;

   ----------------------------------------------------------------------------
   procedure Program_Help (
      Options                    : in      Options_Type;  -- only used for dispatch
      Help_Mode                  : in      ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

   begin
      Log_In (Debug, "mode " & Help_Mode'img);
      case Help_Mode is

      when ADA_LIB.Options.Program =>
         Put_Line (Ada.Command_Line.Command_Name & " options");
         Ada_Lib.Help.Add_Option ('c', "", "use CURL");
         Ada_Lib.Help.Add_Option ('m', "path", "message file path");
         Ada_Lib.Help.Add_Option ('p', "path", "parameter file path");
         Ada_Lib.Help.Add_Option ('u', "name", "user");
         Ada_Lib.Help.Add_Option ('w', "password", "password");

      when ADA_LIB.Options.Traces =>
         null;
         Put_Line ("alias_list (-t)");
         Put_Line ("      a               all");
         Put_Line ("      r               runtime options");
         Put_Line ("      t               aliase_list");

      end case;
      ADA_LIB.Options.Options_Type (Options).Program_Help (Help_Mode);
      Log_Out (Debug);
   end Program_Help;

-- -------------------------------------------------------------------------
-- procedure Set_GNOGA_Debug (
--   State                      : in      Boolean) is
-- -------------------------------------------------------------------------
--
-- begin
--    GNOGA.Debug := True;
-- end Set_GNOGA_Debug;

begin
--Elaborate := True;
--Debug := True;
--Trace_Options := True;
   Log_In (Elaborate or Debug);
   Options := Protected_Options'access;
   Initialize;
   Log_Out (Elaborate or Debug, "options " & Image (Options.all'address));

exception
   when Fault: others =>
      Trace_Exception (Fault);
      ADA_LIB.OS.Immediate_Halt (0);

end Runtime_Options;
