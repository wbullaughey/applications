with Ada.Exceptions;
--with Ada.Strings.Fixed;
with Ada.Text_IO;use Ada.Text_IO;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Base;
with Camera.Lib.Unit_Test;
with Configuration.Camera.State;
--with GNAT.Sockets;
with Gnoga_Ada_Lib;

package body Camera.Lib.Base.Test is

-- use type Camera.Lib.Unit_Test.With_Camera_Test_Type;

   type Test_Type (
      Brand       : Brand_Type) is new
                     Camera.Lib.Unit_Test.With_Camera_Test_Type (
                        Brand) with null record;

   type Test_Access is access Test_Type;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

-- procedure Open (
--    Camera                     : in
--                                           General_Camera_Class_Access;
--    Port                       : in     Port_Type);

-- procedure Port_Scan(
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Read_Write (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   overriding
   procedure Set_Up (Test : in out Test_Type);

-- overriding
-- procedure Tear_Down (Test : in out Test_Type);

   procedure Test_Open (
      Test                       : in out AUnit.Test_Cases.Test_Case'class)
   with Pre => Camera.Lib.Unit_Test.With_Camera_Test_Type (
                  Test).Have_Camera and then
               Camera.Lib.Unit_Test.With_Camera_Test_Type (
                  Test).Have_Camera_Address;

-- function URL return String;

-- procedure URL_Scan(
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- ALPTOP_IP_Address              : constant GNAT.Sockets.Inet_Addr_V4_Type :=
--                                  (192, 168, 1, 240);
-- Camera_Description            : aliased constant String := "test camera";
-- Port                          : constant Port_Type := 80;
-- PTZ_Optics_Local_IP_Address   : GNAT.Sockets.Inet_Addr_Type;
-- PTZ_Optics_Local_IP_Address   : constant GNAT.Sockets.Inet_Addr_V4_Type :=
--                                  (192, 168, 1, 201);
-- PTZ_Optics_Local_URL_Address  : constant String := "192.168.1.201";
-- PTZ_Optics_Port               : constant := 5678;
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
--    Port                       : in     Port_Type) is
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

--   ---------------------------------------------------------------
--   procedure Port_Scan(
--      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--   ---------------------------------------------------------------
--
----    Options                    : Standard.Camera.Lib.Unit_Test.
----                                  Unit_Test_Program_Options_Type'class
----                                     renames Standard.Camera.Lib.Unit_Test.
----                                        Options.all;
--      First_Port                 : constant Port_Type := 1;
--      Last_Port                  : constant Port_Type := 9999;
--      Local_Test                 : Test_Type renames Test_Type (Test);
--
--   begin
--      Put_Line ("test port scan");
--      for Port in First_Port .. Last_Port loop
--         begin
--            Log_Here (Debug, "Port" & Port'img);
--            Local_Test.Camera_Info.Camera.Open (Local_Test.Camera_Info.Camera_Address.all, Port);
--            Local_Test.Camera_Info.Camera.Close;
--            Put_Line ("opened port" & Port'img);
--
--         exception
--            when Standard.Camera.Lib.Base.Failed =>
--               null;
--         end;
--
--         Local_Test.Camera_Info.Camera.Close;
--      end loop;
--      Put_Line ("port scan completed");
--
--   exception
--      when Fault: others =>
--         Trace_Exception (Debug, Fault);
--         Assert (False, Ada.Exceptions.Exception_Message (Fault));
--
--   end Port_Scan;

   ---------------------------------------------------------------
   procedure Read_Write(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      type Ports_Type            is array (Positive range <>) of Port_Type;
      type Ports_Access          is access constant Ports_Type;

      Connection_Data            : constant Standard.Base.Connection_Data_Access :=
                                    Standard.Base.Connection_Data_Access (
                                       Gnoga_Ada_Lib.Get_Connection_Data);
--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
      Local_Test                 : Test_Type renames Test_Type (Test);
      ALPTOP_Ports                : aliased constant Ports_Type := (
                                       554, 1935
                                    );
--    Bad_Port                   : constant Port_Type := 0;
      State                      : Configuration.Camera.State.State_Type renames
                                    Connection_Data.State;
      PTZ_Optics_Ports           : aliased constant Ports_Type := (
                                       1 => State.Get_Host_Port
                                    );
--    Good_Response              : Boolean := False;
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
               Port              : in     Port_Type) is
            ------------------------------------------------------------

               Buffer            : Ada_Lib.Socket_IO.Buffer_Type (1 .. 100);
               Get_Ack           : Boolean;
               Has_Response      : Boolean;
               Next_Buffer_Start : Video.Lib.Index_Type;
               Response_Length   : Index_Type;
               Value             : Data_Type;

            begin
               Log_In (Debug, "port" & Port'img);
               Local_Test.Camera_Info.Camera.Send_Command (
                  Command        => Position_Request,
                  Options        => Standard.Camera.Lib.Base.Null_Option,
                  Get_Ack        => Get_Ack,
                  Has_Response   => Has_Response,
                  Response_Length=> Response_Length);
               delay 0.5;  -- let camera send response
               Log_Here (Debug, "get ack " & Get_Ack'img &
                  " has response " & Has_Response'img &
                  " response length" & Response_Length'img);

               Assert (not Get_Ack, "no ack expected");
               Assert (Has_Response, "response expected");
--             Assert (Response_Length = , "response expected");

               Local_Test.Camera_Info.Camera.Get_Response (
                  Expect_Ack        => Get_Ack,
                  Expect_Response   => Has_Response,
                  Response          => Buffer,
                  Response_Length   => Response_Length,
                  Response_Timeout  => 0.5);

               Local_Test.Camera_Info.Camera.Process_Response (
                  Response          => Buffer,
                  Value             => Value,
                  Next_Buffer_Start => Next_Buffer_Start);

               Log_Here (Debug, "Next_Buffer_Start" & Next_Buffer_Start'img);

               Log_Out (Debug);
            end Test_Port;
            ------------------------------------------------------------

         begin
            Log_Here (Debug, "Port" & Ports.all (Port)'img);
            Local_Test.Camera_Info.Camera.URL_Open (
               Local_Test.Camera_Info.Camera_Address.URL_Address.Coerce,
               Local_Test.Camera_Info.Port_Number);
            Test_Port (Ports.all (Port));
            Local_Test.Camera_Info.Camera.Close ;

         exception
            when Fault: Ada_Lib.Socket_IO.Stream_IO.Timeout =>
               Local_Test.Camera_Info.Camera.Close ;
               Trace_Exception (Debug, Fault);
               Assert (False, "timeout reading response " &
                  Ada.Exceptions.Exception_Message (Fault));

         end;

      end loop;

      Put_Line ("read write completed");
--    Assert (Good_Response, "Port" & Bad_Port'img & " no valid response");
      Log_Out (Debug);

-- exception
--    when Fault: AUnit.Assertions =>
--       Trace_Exception (Debug, Fault);
--       raise;

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

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Port_Scan'access,
--       Routine_Name   => AUnit.Format ("Port_Scan")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Read_Write'access,
         Routine_Name   => AUnit.Format ("Read_Write")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => URL_Scan'access,
--       Routine_Name   => AUnit.Format ("URL_Scan")));

      Log_Out (Debug);
   end Register_Tests;

 ---------------------------------------------------------------
 Overriding
 procedure Set_Up (Test : in out Test_Type) is
 ---------------------------------------------------------------

 begin
    Log_Here (Debug or Trace_Set_Up);
    Test.Camera_Info.Open_Camera := False;
    Camera.Lib.Unit_Test.With_Camera_Test_Type (Test).Set_Up;
 end Set_Up;

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
                                    Brand       => Options.Camera_Options.Brand);

   begin
      Log_In (Debug, "brand " & Options.Camera_Options.Brand'img);
      Ada_Lib.Unit_Test.Suite (Suite_Name);  -- used for listing suites
      Test_Suite.Add_Test (Test);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

-- ---------------------------------------------------------------
-- procedure Tear_Down (Test : in out Test_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log (Debug, Here, Who);
--    Test.Camera_Info.Camera.Close;
--    Ada_Lib.Unit_Test.Test_Cases.Test_Case_Type (Test).Tear_Down;
-- end Tear_Down;

   ---------------------------------------------------------------
   procedure Test_Open(
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

--    Options                    : Standard.Camera.Lib.Unit_Test.
--                                  Unit_Test_Program_Options_Type'class
--                                     renames Standard.Camera.Lib.Unit_Test.
--                                        Get_Camera_Unit_Test_Constant_Options.all;
      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Put_Line ("test open");
      Local_Test.Camera_Info.Camera.Open (Local_Test.Camera_Info.Camera_Address.all,
         Local_Test.Camera_Info.Port_Number);

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

--   ---------------------------------------------------------------
--   procedure URL_Scan(
--      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--   ---------------------------------------------------------------
--
----    Options                    : Standard.Camera.Lib.Unit_Test.
----                                  Unit_Test_Program_Options_Type'class
----                                     renames Standard.Camera.Lib.Unit_Test.
----                                        Get_Camera_Unit_Test_Constant_Options.all;
--      First_Unit                 : constant GNAT.Sockets.Inet_Addr_Comp_Type := 2;
----    IP_Address                 : GNAT.Sockets.Inet_Addr_V4_Type :=
----                                  (192, 168, 1, 0);
--      Last_Unit                  : constant GNAT.Sockets.Inet_Addr_Comp_Type := 254;
--      Local_Test                 : Test_Type renames Test_Type (Test);
--
--   begin
--      Put_Line ("test unit scan");
--      for Unit in First_Unit .. Last_Unit loop
----       IP_Address (4) := Unit;
--         begin
--            Local_Test.Camera_Info.Camera.Open (Local_Test.Camera_Info.Camera_Address.all, Unit);
--            Put_Line ("opened unit" & Unit'img);
--
--         exception
--            when Failed =>
--               null;
--         end;
--
--         Local_Test.Camera_Info.Camera.Close;
--      end loop;
--      Put_Line ("unit scan completed");
--
--   exception
--      when Fault: others =>
--         Trace_Exception (Debug, Fault);
--         Assert (False, Ada.Exceptions.Exception_Message (Fault));
--
--   end URL_Scan;

begin
   if Trace_Tests then
      Debug := Trace_Tests;
   end if;

end Camera.Lib.Base.Test;
