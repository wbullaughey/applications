with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;use Ada.Text_IO;
--with Ada_Lib.Test.Tests;
--with Ask;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with AUnit.Test_Suites;
--with Ada_Lib.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Ada_Lib.Unit_Test.Test_Cases;
with GNAT.Sockets;
with Hex_IO;
--with Camera.Commands.PTZ_Optics;
--with Camera.LIB.ALPTOP;
with Camera.Lib.Unit_Test;

package body Camera.Lib.Base.Test is

   use type Index_Type;

   type Test_Type (
      Brand             : Brand_Type) is new
                           Camera.Lib.Unit_Test.Camera_Test_Type (Brand) with null record;

--    Camera            : Base.Base_Camera_Class_Access := Null;
--    case Brand is
--       when ALPTOP_Camera =>
--          ALPTOP      : aliased Standard.Camera.LIB.ALPTOP.ALPTOP_Type;
--
--       when No_Camera=>
--          Null;
--
--       when PTZ_Optics_Camera =>
--          PTZ_Optics  : aliased Standard.Camera.Commands.PTZ_Optics.
--                         PTZ_Optics_Type;
--
--    end case;
-- end record;

   type Test_Access is access Test_Type;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

-- procedure Open (
--    Camera                     : in
--                                           General_Camera_Class_Access;
--    Port                       : in     GNAT.Sockets.Port_Type);

   procedure Port_Scan(
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Read_Write (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

-- overriding
-- procedure Set_Up (Test : in out Test_Type)

-- overriding
-- procedure Tear_Down (Test : in out Test_Type);

   procedure Test_Open (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- function URL return String;

   procedure URL_Scan(
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- ALPTOP_IP_Address              : constant GNAT.Sockets.Inet_Addr_V4_Type :=
--                                  (192, 168, 1, 240);
-- Port                          : constant GNAT.Sockets.Port_Type := 80;
-- PTZ_Optics_Local_IP_Address   : GNAT.Sockets.Inet_Addr_Type;
-- PTZ_Optics_Local_IP_Address   : constant GNAT.Sockets.Inet_Addr_V4_Type :=
--                                  (192, 168, 1, 201);
-- PTZ_Optics_Local_URL_Address  : constant String := "192.168.1.201";
   PTZ_Optics_Port               : constant := 5678;
-- PTZ_Optics_Remote_URL_Address : constant String := "http://ucwc.dyndns.org";
-- Local_Port                    : constant String := ":80";
-- Remote_Port                   : constant String := ":9100";
   Suite_Name                    : constant String := "Basic_Video";


   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Suite_Name);
   end Name;

-- ---------------------------------------------------------------
-- procedure Open (
--    Camera                     : in
--                                           General_Camera_Class_Access;
--    Port                       : in     GNAT.Sockets.Port_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "brand " & Global_Camera_Lib_Options.
--       Brand'img & " port" & Port'img);
--    case Global_Camera_Lib_Local_Test.Brand is
--
--       when ALPTOP_Camera =>
--         Camera.IP_Open (ALPTOP_IP_Address, Port);
--
--       when PTZ_Optics_Camera =>
--         Camera.Open (Global_Camera_Lib_Options.
--             Camera_Address.all, Port);
--
--       when No_Camera =>
--          raise Failed with "no camera set";
--
--    end case;
--    Log_Out (Debug);
-- end Open;

   ---------------------------------------------------------------
   procedure Port_Scan(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Options.all;
      First_Port                 : constant GNAT.Sockets.Port_Type := 1;
      Last_Port                  : constant GNAT.Sockets.Port_Type := 9999;
      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Put_Line ("test port scan");
      for Port in First_Port .. Last_Port loop
         begin
            Local_Test.Camera.Open (Local_Test.Camera_Address.all, Port);
            Put_Line ("opened port" & Port'img);

         exception
            when Failed =>
               null;
         end;

         Local_Test.Camera.Close;
      end loop;
      Put_Line ("port scan completed");

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, Ada.Exceptions.Exception_Message (Fault));

   end Port_Scan;

   ---------------------------------------------------------------
   procedure Read_Write(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      type Ports_Type            is array (Positive range <>) of GNAT.Sockets.Port_Type;
      type Ports_Access          is access constant Ports_Type;

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
      Local_Test                 : Test_Type renames Test_Type (Test);
      ALPTOP_Ports                : aliased constant Ports_Type := (
                                       554, 1935
                                    );
      Bad_Port                   : GNAT.Sockets.Port_Type := 0;
      PTZ_Optics_Ports           : aliased constant Ports_Type := (
                                       1 => 5678
                                    );
      Good_Response              : Boolean := False;
      Ports                      : Ports_Access := Null;

   begin
      Log_In (Debug);
      Put_Line ("read write");

      case Camera.Lib.Unit_Test.Get_Camera_Unit_Test_Constant_Options.
            Camera_Options.Brand is

         when ALPTOP_Camera =>
            Ports := ALPTOP_Ports'access;

         when PTZ_Optics_Camera =>
            Ports := PTZ_Optics_Ports'access;

          when No_Camera =>
            raise Failed with "no camera set";

      end case;
      for Port in Ports.all'range  loop
         declare

            ------------------------------------------------------------
            procedure Test_Port (
               Port              : in     GNAT.Sockets.Port_Type) is
            ------------------------------------------------------------

               Ack_Value         : Natural;
               Buffer            : Ada_Lib.Socket_IO.Buffer_Type (1 .. 100);
               Completion_Value  : Natural;
               Get_Ack           : Boolean;
               Has_Response      : Boolean;
               Next_Byte         : Index_Type;
               Response_Length   : Index_Type;

            begin
               Log_In (Debug);
               Local_Test.Camera.Send_Command (
                  Command        => Memory_Recall,
                  Options        => ( 1 => (
                     Data           => 3,            -- set recall to setting 3
                     Start          => 6,
                     Variable_Width => False)),
                  Get_Ack        => Get_Ack,
                  Has_Response   => Has_Response,
                  Response_Length=> Response_Length);
               delay 0.5;  -- let camera send response

               Assert (not Has_Response, "no response expected");
               if Get_Ack then
                  declare
                     Ack_Length  : constant Index_Type :=
                                    Local_Test.Camera.Get_Ack_Length;
                  begin
                     Local_Test.Camera.Get_Response (Get_Ack, Has_Response,
                        Buffer, Response_Length,
                        Local_Test.Camera.Get_Timeout (Memory_Recall));

                     Local_Test.Camera.Acked (Buffer, Ack_Value, Next_Byte);
                     Put_Line ("ack value" & Hex_IO.Hex (Ack_Value, 8) & " next value" & Next_Byte'img);
                     if Next_Byte > 0 then
                        Local_Test.Camera.Completed (
                              Buffer (Next_Byte .. Ack_Length),
                              3, Completion_Value, Next_Byte);
                        Put_Line ("completion value" & Completion_Value'img &
                           " Next_Byte" & Next_Byte'img);
                     end if;

                     declare
                        Line           : String (1 .. Natural (Ack_Length));
                        for Line'address use Buffer'address;

                     begin
                        if Ada.Strings.Fixed.Index (Line, "Bad Request") = 0 then
                           Put_Line ("command succeded for port" & Port'img);
                           Good_Response := True;
                        else
                           Good_Response := False;
                           Bad_Port := Port;
                           Put_Line ("command failed for port" & Port'img);
                        end if;
                        Log_Out (Debug, "response length" & Ack_Length'img &
                           " Good_Response " & Good_Response'img);
                     end;
                  end;
               end if;
               Assert (not Has_Response, "response not expected");
               Assert (Response_Length = 0, "Response_Length should be 0");
            end Test_Port;
            ------------------------------------------------------------

         begin
            Log_Here (Debug, "Port" & Ports.all (Port)'img);
            Local_Test.Camera.URL_Open (
               Local_Test.Camera_Address.URL_Address.Coerce,
               Local_Test.Port_Number);
            Test_Port (Ports.all (PTZ_Optics_Port));
            Local_Test.Camera.Close;

         exception
            when Failed =>
               null;
         end;

      end loop;

      Put_Line ("read write completed");
      Assert (Good_Response, "Port" & Bad_Port'img & " no valid response");
      Log_Out (Debug);

-- exception
--    when AUnit.Assertions =>
--       raise;
--
--    when Fault: others =>
--       Trace_Exception (Debug, Fault);
--       Assert (False, Ada.Exceptions.Exception_Message (Fault));

   end Read_Write;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Open'access,
         Routine_Name   => AUnit.Format ("Test_Open")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Port_Scan'access,
         Routine_Name   => AUnit.Format ("Port_Scan")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Read_Write'access,
         Routine_Name   => AUnit.Format ("Read_Write")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => URL_Scan'access,
         Routine_Name   => AUnit.Format ("URL_Scan")));

      Log_Out (Debug);
   end Register_Tests;

-- ---------------------------------------------------------------
-- procedure Set_Up (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
--    Seperator               : character;
--
-- begin
--Log (Here, Who);
--    Log (Debug, Here, Who);
-- end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Test                       : constant Test_Access := new Test_Type (
                                    Options.Camera_Options.Brand);

   begin
      Log_In (Debug, "brand " & Options.Camera_Options.Brand'img);
      Test_Suite.Add_Test (Test);
      case Options.Camera_Options.Brand is

         when ALPTOP_Camera =>
            Test.Camera := Test.ALPTOP'access;

         when No_Camera =>
            raise Failed with "no camera brand selected";

         when PTZ_Optics_Camera =>
            Test.Camera := Test.PTZ_Optics'access;

      end case;
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

-- ---------------------------------------------------------------
-- procedure Tear_Down (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log (Debug, Here, Who);
--    Test.Camera.Close;
--    Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
-- end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Open(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Put_Line ("test open");
      Local_Test.Camera.Open (Options.Camera_Options.Camera_Address.all, 80);

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, Ada.Exceptions.Exception_Message (Fault));

   end Test_Open;

-- ---------------------------------------------------------------
-- function URL return String is
-- ---------------------------------------------------------------
--
-- begin
--    return (case Global_Camera_Lib_Options.Location is
--
--       when Configuration.State.Local =>
--          Global_Camera_Lib_Options.
--            URL_Address.Coerce &
--             Global_Camera_Lib_Options.Port_Number'img,
--
--       when Configuration.State.Remote =>
--          PTZ_Optics_Remote_URL_Address & Remote_Port);
-- end URL;

   ---------------------------------------------------------------
   procedure URL_Scan(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Unit_Test_Program_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Unit_Test_Constant_Options.all;
      First_Unit                 : constant GNAT.Sockets.Inet_Addr_Comp_Type := 2;
--    IP_Address                 : GNAT.Sockets.Inet_Addr_V4_Type :=
--                                  (192, 168, 1, 0);
      Last_Unit                  : constant GNAT.Sockets.Inet_Addr_Comp_Type := 254;
      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Put_Line ("test unit scan");
      for Unit in First_Unit .. Last_Unit loop
--       IP_Address (4) := Unit;
         begin
            Local_Test.Camera.Open (Camera.Lib.Unit_Test.
               Get_Camera_Unit_Test_Constant_Options.Camera_Options.
                  Camera_Address.all, 80);     -- use default HTTP port
            Put_Line ("opened unit" & Unit'img);

         exception
            when Failed =>
               null;
         end;

         Local_Test.Camera.Close;
      end loop;
      Put_Line ("unit scan completed");

   exception
      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Assert (False, Ada.Exceptions.Exception_Message (Fault));

   end URL_Scan;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Camera.Lib.Base.Test;
