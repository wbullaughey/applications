--with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Help;
with Ada_Lib.Options.Create;
with Ada_Lib.Options.Runstring;
with ADA_LIB.String_Quote; use ADA_LIB.String_Quote;
with ADA_LIB.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Lib.Options;

package body Camera is

-- use type Ada_Lib.Options.Options_Type;
-- use type Video.Lib.Location_Type;

   Debug                      : Boolean renames
                                 Lib.Options.Camera_Options.Camera_Debug;
   Trace_Flag                 : constant Character := '3';
   Trace_Option               : constant Character := 'd';
   Options_With_Parameters    : aliased constant
                                 Ada_Lib.Options.Flag_List_Type :=
                                    Ada_Lib.Options.Create.Create_One (
                                       'b', Ada_Lib.Options.Unmodified_Flag);
   Options_Without_Parameters : aliased constant
                                 Ada_Lib.Options.Flag_List_Type :=
                                    Ada_Lib.Options.Create.Create_One (
                                       Trace_Option,  -- local is default
                                       Ada_Lib.Options.Unmodified_Flag);
   Recursed                   : Boolean := False;

   ----------------------------------------------------------------
-- overriding
   function Camera_Hash (
      Address                    : in     Address_Type
   ) return Ada.Containers.Hash_Type is
   pragma Unreferenced (Address);
   ----------------------------------------------------------------

   begin
not_implemented;
return 0;
   end Camera_Hash;

   ----------------------------------------------------------------
   function Camera_ID_Equal (
      Left, Right                : in     Camera_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end Camera_ID_Equal;

   ----------------------------------------------------------------
   function Camera_ID_Hash (
      Key                        : in     Camera_ID_Type
   ) return Ada.Containers.Hash_Type is
   ----------------------------------------------------------------

   begin
      return Key.Value;
   end Camera_ID_Hash;

   -------------------------------------------------------------------------
   procedure Dump (
      Camera_ID                  : in        Camera_ID_Type) is
   -------------------------------------------------------------------------

   begin
      Put_Line ("camera id: " & Camera_ID.Image);
   end Dump;

   -------------------------------------------------------------------------
   function Has_Location (
      Location       : in     Configuration.State.Location_Type
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      return Location /= Video.Lib.No_Location;
   end Has_Location;

   -------------------------------------------------------------------------
   function Image (
      Camera_ID                  : in        Camera_ID_Type
   ) return String is
   -------------------------------------------------------------------------

   begin
      return (if Camera_ID.Set then
            "hash:" & Camera_ID.Value'img
         else
            "not set");
   end Image;

   -------------------------------------------------------------------------
   overriding
   function Initialize (
      Options               : in out Camera_Options_Type;
      From                  : in     String := Ada_Lib.Trace.Here
   ) return Boolean is
   -------------------------------------------------------------------------

   begin
      Log_In_Checked (Recursed, Debug or Trace_Options,
         "Without Parameters " & Options_Without_Parameters.Image &
         " from " & From);

      Ada_Lib.Options.Runstring.Options.Register (
         Ada_Lib.Options.Runstring.Without_Parameters,
         Options_Without_Parameters);

      return Log_Out_Checked (Recursed,
         Video.Lib.Options_Type (Options).Initialize,
         Debug or Trace_Options);
   end Initialize;

   ----------------------------------------------------------------------------
   function Is_Set (
      Camera_ID                  : in        Camera_ID_Type
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      return Camera_ID.Set;
   end Is_Set;

   ----------------------------------------------------------------
   function Make_Camera_ID (
      Address     : in        Address_Type
   ) return Camera_ID_Type is
   ----------------------------------------------------------------

      Result      : Camera_ID_Type;

   begin
      Log_In (Debug, "camera address " & Address.Image);
      case Address.Address_Kind is

         when Ada_Lib.Socket_IO.IP =>
            declare
               type Unsigned_64 is mod 2**64;
               Accumulator : Unsigned_64 := 0;
               IP_Address  : Ada_Lib.Socket_IO.IP_Address_Type renames
                              Address.IP_Address;
            begin
               for Segment of IP_Address  loop
                  Accumulator := (Accumulator * 256 + Unsigned_64 (Segment)) mod
                     Unsigned_64 (Natural'last);
               end loop;
               Result.Value := Ada.Containers.Hash_Type (Accumulator);
               Result.Set := True;
            end;

         when Ada_Lib.Socket_IO.URL =>
            Result.Value := Ada.Strings.Hash (Address.URL_Address.Coerce);
            Result.Set := True;

         when Ada_Lib.Socket_IO.Not_Set =>
            pragma Assert (False, "pre should prevent this");

      end case;

      Log_Out (Debug, "IP Address " &
         Ada_Lib.Socket_IO.Image (Address) & ": " & Result.Image);
      return Result;
   end Make_Camera_ID;

   ----------------------------------------------------------------------------
   -- processes options it knows about and calls parent for others
   overriding
   function Process_Option (
      Options  : in out Camera_Options_Type;
      Iterator : in out Ada_Lib.Options.Command_Line_Iterator_Interface'class;
      Option   : in     Ada_Lib.Options.Base_Flag_Option_Type'class
   ) return Boolean is
   ----------------------------------------------------------------------------

   begin
      Log_In (Trace_Options or Debug, Option.Image);
--       " help test " & Ada_Lib.Options.Help_Test'img);

      if Ada_Lib.Options.Has_Option (Option, Options_With_Parameters,
            Options_Without_Parameters) then
         case Option.Option is

            when 'b' =>
               declare
                  Parameter      : constant String := Iterator.Get_Parameter;

               begin
                  Options.Brand := (if Parameter = "PTZ_Optics" then
                        PTZ_Optics_Camera
                     else (if Parameter = "ALPTOP" then
                        ALPTOP_Camera
                     else
                        No_Camera));

                  if  Options.Brand  = No_Camera then
                     Options.Bad_Option (Parameter,
                        Quote ("Invalid camera type", Parameter));
                  end if;
               end;

            when Trace_Option =>
               Options.Trace_Parse (Iterator);


            when others =>
               declare
                  Message  : constant String :=
                              "Has_Option incorrectly passed " & Option.Image;
               begin
                  Log_Exception (Trace_Options or Debug, Message);
                  raise Failed with Message;
               end;
         end case;

         return Log_Out (Video.Lib.Options_Type (Options).Process_Option (
               Iterator, Option),
            Debug or Trace_Options,
            Option.Image & " handled");
      else
         return Log_Out (
            Video.Lib.Options_Type (
               Options).Process_Option (Iterator, Option),
               Trace_Options or Debug, "other " & Option.Image);
      end if;

   exception
      when Fault: others =>
         Trace_Exception (Fault);
         raise;

   end Process_Option;

   ----------------------------------------------------------------------------
   overriding
   procedure Program_Help (
      Options                    : in     Camera_Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type) is
   ----------------------------------------------------------------------------

      Component                  : constant String := "Camera";

   begin
      Log_In (Debug or Trace_Options, "help mode " & Help_Mode'img);

        case Help_Mode is

        when Ada_Lib.Options.Program_Mode =>
           Log_Here (Debug or Trace_Options, Quote ("Component", Component));
           Ada_Lib.Help.Create_Option (Trace_Flag, "trace options", "Camera Lib Debug",
              Component, Ada_Lib.Help.Unmodified_Flag);
           New_Line;

        when Ada_Lib.Options.Trace_Mode =>
           New_Line;

           Put_Line (Component & " trace options (-" &
              Trace_Flag& ")");
           Put_Line ("      d               Camera.Camera_Debug");
        end case;
--
      Video.Lib.Options_Type (Options).Program_Help (
         Help_Mode);
      Log_Out (Debug or Trace_Options);
   end Program_Help;

   ----------------------------------------------------------------------------
   overriding
   procedure Trace_Parse (
      Options        : in out Camera_Options_Type;
      Iterator       : in out Ada_Lib.Options.
                                 Command_Line_Iterator_Interface'class) is
   ----------------------------------------------------------------------------

--    type Suboption_Type        is (Plain, Modified);

      Parameter                  : constant String := Iterator.Get_Parameter;
--    Suboption                  : Suboption_Type := Plain;

   begin
      Log (Trace_Options or Debug, Here, Who & Quote (" Parameter", Parameter));

      for Trace of Parameter loop
         Log_Here (Trace_Options or Debug, Quote ("Trace", Trace));

         case Trace is

            when 'a' =>
               Debug := True;

            when Trace_Option =>
               Debug := True;

            when others =>
               Options.Bad_Option (Quote (
                  "unexpected trace option", Trace) &
                  " for '" & Trace_Option & "'");

         end case;

      end loop;
   end Trace_Parse;

begin
--Elaborate := True;
--Trace_Options := True;
--Trace_Pre_Post_Conditions := True;
   Log_Here (Elaborate or else Trace_Options or else Trace_Pre_Post_Conditions);
end Camera;
