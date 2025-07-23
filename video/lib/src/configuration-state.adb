with Ada.Exceptions;
with Ada_Lib.Parser;
with Ada_Lib.Strings.Unlimited; use Ada_Lib.Strings; use Ada_Lib.Strings.Unlimited;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_Lib.Trace; use Ada_Lib.Trace;

package body Configuration.State is

   Address_Key                   : constant Address_Key_Type := (
                                    Local    => new String'("local_camera"),
                                    Remote   => new String'("remote_camera"));
   Port_Key                      : constant Address_Key_Type := (
                                    Local    => new String'("local_port"),
                                    Remote   => new String'("remote_port"));

   ----------------------------------------------------------------
   procedure Dump (
      State                      : in     State_Type) is
   ----------------------------------------------------------------

   begin
      Put_Line (Quote ("  Local address:", State.Video_Address.Image));
      Put_Line ("  Local port:" & State.Video_Port'img);
   end Dump;

   ----------------------------------------------------------------
   function Get_Host_Address (
      State                      : in     State_Type
   ) return Ada_Lib.Socket_IO.Address_Type is
   ----------------------------------------------------------------

   begin
      return State.Video_Address.all;
   end Get_Host_Address;

   ----------------------------------------------------------------
   function Get_Host_Port (
      State                      : in     State_Type
   ) return Video.Lib.Port_Type is
   ----------------------------------------------------------------

   begin
      return State.Video_Port;
   end Get_Host_Port;

   ----------------------------------------------------------------
   procedure Load (
      State                      : in out State_Type;
      Config                     : in     Ada_Lib.Configuration.Configuration_Type;
      Location                   : in     Location_Type;
      File_Name                  : in     String) is
   ----------------------------------------------------------------

      Reported                   : exception;

      -------------------------------------------------------------
      function Get_Config_String (
         Key                     : in     String
      ) return String is
      -------------------------------------------------------------

      begin
         return Config.Get_String (Key);

      exception
         when Fault: Ada_Lib.Configuration.Failed =>
            Trace_Exception (Debug, Fault);
            Put_Line (Key & Quote (" not found in configuration file", File_Name));
            raise Reported;

      end Get_Config_String;

      -------------------------------------------------------------
      function Get_Parser_String (
         Parser                  : in out Ada_Lib.Parser.Iterator_Type;
         Field                   : in     String;
         Do_Next                 : Boolean
      ) return String is
      -------------------------------------------------------------

      begin
         return Parser.Get_Value (Do_Next);

      exception
         when Fault: Ada_Lib.Parser.Underflow =>
            Trace_Exception (Debug, Fault);
            Put_Line ("Missing " & Field &
               Quote (" in configuration line", Parser.Get_Original) &
               Quote (" in ", File_Name));
            raise Reported;

      end Get_Parser_String;

      -------------------------------------------------------------
      function Load_Address
      return Ada_Lib.Socket_IO.Address_Type is
      -------------------------------------------------------------

         Address                  : constant String :=
                                     Get_Config_String (Address_Key (Location).all);
         Parser                   : Ada_Lib.Parser.Iterator_Type :=
                                     Ada_Lib.Parser.Initialize (
                                        Value          => Address,
                                        Seperators     => ",");
         Kind                     : constant String := Get_Parser_String (
                                       Parser, "camera address type",
                                       Do_Next => True);
         Camera_Address          : constant String := Get_Parser_String (
                                       Parser, "camera address",
                                       Do_Next => False);
      begin
         Log_In (Debug, "location " & Location'img &
            Quote (" address", Address) & Quote (" kind", Kind) &
            " Camera_Address " & Camera_Address);
         if Kind = "IP" then
            declare
               Result            : Ada_Lib.Socket_IO.Address_Type (
                                    Ada_Lib.Socket_IO.IP);
               IP_Parser         : Ada_Lib.Parser.Iterator_Type :=
                                     Ada_Lib.Parser.Initialize (
                                        Value          => Camera_Address,
                                        Seperators     => ".");

            begin
               for Segment in Result.IP_Address'range loop
                  Log_Here (Debug, Segment'img);
                  declare
                     Value       : Ada_Lib.Socket_IO.IP_Address_Segment_Type
                                    renames Result.IP_Address (Segment);
                  begin
                     Value := Ada_Lib.Socket_IO.IP_Address_Segment_Type'value (
                        Get_Parser_String (IP_Parser,
                        "IP setment" & Segment'img, Do_Next => True));
                  end;
               end loop;
               Log_Out (Debug, "ip address" & Result.Image);
               return Result;
            end;
         elsif Kind = "URL" then
            declare
               Result            : Ada_Lib.Socket_IO.Address_Type (
                                    Ada_Lib.Socket_IO.URL);

            begin
               Result.URL_Address.Append (Camera_Address);
               Log_Out (Debug, "url address " & Result.Image);
               return Result;
            end;
         else
            Log_Exception (Debug);
            raise Failed with Quote ("Invalid camera address prefix at " &
               Here, Kind);
         end if;

      exception
         when Fault: Ada_Lib.Parser.Underflow =>
            Trace_Exception (Debug, Fault);
            Put_Line ("Invalid configuration file " &
               Ada.Exceptions.Exception_Message (Fault));
            raise;

      end Load_Address;
      -------------------------------------------------------------

   begin
      Log_In (Debug);

      declare
         Video_Address           : constant Ada_Lib.Socket_IO.Address_Type :=
                                    Load_Address;
      begin
         State.Video_Address := new Ada_Lib.Socket_IO.Address_Type'(Video_Address);

         State.Video_Port := Video.Lib.Port_Type'value (
            Get_Config_String (Port_Key (Location).all));

         Log_Out (Debug);
      end;

   exception

      when Fault: Ada_Lib.Configuration.Failed =>
         Trace_Exception (Debug, Fault);
         Put_Line ("Invalid configuration file: " &
            Ada.Exceptions.Exception_Message (Fault));
         raise;

      when Fault: Ada_Lib.Parser.Underflow =>
         Trace_Exception (Debug, Fault);
         Put_Line ("Invalid configuration file " &
            Ada.Exceptions.Exception_Message (Fault));
         raise;

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Load;

   ----------------------------------------------------------------
   procedure Load (
      State                      : in out State_Type;
      Location                   : in     Location_Type;
      Name                       : in     String) is
   ----------------------------------------------------------------

      Config                     : Ada_Lib.Configuration.Configuration_Type;

   begin
      Log_In (Debug, Quote ("name", Name) & " location " & Location'img);

      Config.Load (Name, False);
      State.Load (Config, Location, Name);

      Log_Here (Debug,
         Quote ("video address", State.Video_Address.Image) &
         Quote ("video port", State.Video_Port'img));

      State.Loaded := True;
      Log_Out (Debug);

   exception

      when Fault: Ada_Lib.Configuration.Failed =>
         Trace_Exception (Debug, Fault);
         Put_Line ("Invalid configuration file: " &
            Ada.Exceptions.Exception_Message (Fault));
         raise;

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         raise;

   end Load;

   -------------------------------------------------------------
   procedure Unload (
      State                      : in out State_Type) is
   -------------------------------------------------------------

   begin
      State.Loaded := False;
   end Unload;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);

end Configuration.State;
