with Ada.Text_IO; use Ada.Text_IO;
--with Ada_Lib.GNOGA;
--with Ada_Lib.Time;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Lib.Base;
--with Camera.Command_Queue;
--with Camera.Lib.Connection;
with Camera.Lib.Unit_Test;
with Interfaces;
with Video.Lib;

package body Camera.Lib.Base.Command_Tests is

-- use type Ada.Streams.Stream_Element;
-- use type Ada_Lib.Time.Time_Type;
   use type Interfaces.Integer_16;
-- use type Interfaces.Unsigned_16;
   use type Preset_ID_Type;

   type Raw_Test_Type (
      Brand : Brand_Type) is new Camera.Lib.Unit_Test.Camera_Test_Type (
         Brand) with record
      Manual                     : Boolean := False;
   end record;

   type Raw_Test_Access          is access Raw_Test_Type;

   overriding
   function Name (
      Test                       : in     Raw_Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Raw_Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Raw_Test_Type);

   type Test_Type (
      Brand    : Brand_Type) is new Raw_Test_Type (Brand) with null record;

   type Test_Access              is access Test_Type;

   overriding
   function Name (
      Test                       : in     Test_Type) return AUnit.Message_String;

   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type);

   overriding
   procedure Set_Up (
      Test                       : in out Test_Type);

-- procedure Test_Auto_Focus (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);
--
-- procedure Test_Manual_Focus (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Position_Absolute (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Down_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Down_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Straight_Down (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Request (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Stop (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Straight_Up (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Up_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Up_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Power (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Update_Preset (      -- tests Memory_Recall
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Reset_Memory (       -- tests Memory_Reset
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Move_To_Preset (         -- tests Memory_Set - sets camera to preset
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

-- procedure Test_Recall_Speed (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);
--
-- procedure Test_Zoom_Direct (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);
--
-- procedure Test_Zoom_Full (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Wait (
      Length                     : in     Duration);

   Raw_Suite_Name                : constant String := "Raw_Video_Commands";
   Suite_Name                    : constant String := "Video_Commands";

--   ---------------------------------------------------------------
--   procedure Get_Absolute (
--      Camera                     : in out Base_Camera_Type'class;
--      Pan                        :    out Absolute_Type;
--      Tilt                       :    out Absolute_Type) is
--   ---------------------------------------------------------------
--
--      Accumulator                : Interfaces.Unsigned_16;
--      Conversion                 : Absolute_Type;
--      for Conversion'address use Accumulator'address;
--      Response_Buffer            : Maximum_Response_Type;
--      Response_Length            : Index_Type;
--
--   begin
--      Log_In (Debug);
--      Camera.Process_Command (Position_Request,
--         Options           => Null_Options,
--         Response          => Response_Buffer,
--         Response_Length   => Response_Length);
--
----    if Debug then
----       Response.Dump;
----    end if;
--
--      Accumulator := 0;
--      for I in Index_Type'(3) .. 6 loop
--         Log_Here (Debug, I'img & ": " &
--            Ada_lib.Socket_IO.Hex (Response_Buffer (I)));
--         Accumulator := Accumulator * 16#10# +
--            Interfaces.Unsigned_16 (Response_Buffer (I) and 16#F#);
--      end loop;
--      Log_Here (Debug, Hex_IO.Hex (Accumulator) &
--         " conversion" & Conversion'img);
--      Pan := Conversion;
--
--      Accumulator := 0;
--      for I in Index_Type'(7) .. 10 loop
--         Log_Here (Debug, I'img & ": " &
--            Ada_lib.Socket_IO.Hex (Response_Buffer (I)));
--         Accumulator := Accumulator * 16#10# +
--            Interfaces.Unsigned_16 (Response_Buffer (I) and 16#F#);
--      end loop;
--      Log_Here (Debug, Hex_IO.Hex (Accumulator) &
--         " conversion" & Conversion'img);
--      Tilt := Conversion;
--   end Get_Absolute;

   ---------------------------------------------------------------
   procedure Get_Absolute_Iterate (
      Test              : in out AUnit.Test_Cases.Test_Case'class) is
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type) is
   ---------------------------------------------------------------

      Last_Pan                   : Absolute_Type := Absolute_Type'last;
      Last_Tilt                  : Absolute_Type := Absolute_Type'last;
      Local_Test        : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      loop
         Local_Test.Command_Queue.Camera.Get_Absolute (Pan, Tilt);
      end loop;
   end Get_Absolute_Iterate;

   ---------------------------------------------------------------
   function Get_Power (
      Test              : in out Raw_Test_Type'class
   ) return Boolean is
   ---------------------------------------------------------------

      Response_Buffer      : Maximum_Response_Type;
      Response_Length      : Index_Type;
      Result               : Boolean;

   begin
      Log_In (Debug);
      Test.Camera_Queue.Process_Command (Power_Request,
         Options           => Null_Options,
         Response          => Response_Buffer,
         Response_Length   => Response_Length);

      case Response_Buffer (3) is

         when 2 =>
            Result := True;

         when 3 =>
            Result := False;

         when 4 =>
            raise Failed with "camera power failure code" &
               Response_Buffer (Response_Buffer'first)'img;

         when others =>
            raise Failed with "unexpected power value" &
               Response_Buffer (Response_Buffer'first)'img;

      end case;
      return Log_Out (Result, Debug, "Power " & Result'img);
   end Get_Power;

   ---------------------------------------------------------------
   procedure Move_To_Preset (         -- tests Memory_Set - sets camera to preset
      Test                       : in out Raw_Test_Type'class;
      Set_Point                  : in     Preset_ID_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Move to Preset" & Set_Point'img);
      Test.Camera_Queue.Process_Command (Memory_Recall,
         Options           => ( 1 =>
               (
                  Data           => Data_Type (Set_Point),
                  Start          => 6,
                  Variable_Width => False
               )
            ));
      delay 2.0;     -- wait for camera to reposition
      Log_Out (Debug);
   end Move_To_Preset;

   ---------------------------------------------------------------
   overriding
   function Name (
      Test                       : in     Raw_Test_Type) return AUnit.Message_String is
   pragma Unreferenced (Test);
   ---------------------------------------------------------------

   begin
      return AUnit.Format (Raw_Suite_Name);
   end Name;

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
-- procedure Position_Relative (
--    Camera                     : in out Base_Camera_Type'class;
--    Pan                        : in     Relative_Type;
--    Tilt                       : in     Relative_Type;
--    Pan_Speed                  : in     Property_Type := 1;
--    Tilt_Speed                 : in     Property_Type := 1) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
--    Camera.Process_Command (Position_Relative,
--       Options     => (
--          (
--             Data           => Pan_Speed,
--             Start          => 5,
--             Variable_Width => False
--          ),
--          (
--             Data           => Tilt_Speed,
--             Start          => 6,
--             Variable_Width => False
--          ),
--          (
--             Start          => 7,
--             Variable_Width => True,
--             Value          => Convert (Pan),
--             Width          => 4
--          ),
--          (
--             Start          => 11,
--             Variable_Width => True,
--             Value          => Convert (Tilt),
--             Width          => 4
--          )
--       )
--    );
--
--    Log_Out (Debug);
--
-- exception
--    when Fault : others =>
--       Trace_Exception (Fault, Here);
--       raise;
--
-- end Position_Relative;

   ---------------------------------------------------------------
   procedure Position_Test (
      Test              : in out Raw_Test_Type'class;
      Command           : in     Commands_Type;
      Sideways          : in     String;
      Vertical          : in     String) is
   ---------------------------------------------------------------

      Description       : constant String := Vertical & " " & Sideways;
      Default_Preset    : constant Preset_ID_Type :=
                           Test.Camera_Queue.Get_Default_Preset;
      Options           : Standard.Camera.Lib.Unit_Test.
                           Camera_Lib_Unit_Test_Options_Type'class
                              renames Standard.Camera.Lib.Unit_Test.
                                 Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      type Speeds_Type  is range 1 .. 2;
      Speeds            : constant array (Speeds_Type) of Data_Type := (
                           1,       -- slow speed
                           5);      -- fast speed
      Rate              : constant array (Speeds_Type) of String (1 .. 4) := (
                           String'("slow"),
                           String'("fast"));
      Wait              : constant array (Speeds_Type) of Duration := (
                           2.0,  -- slow time
                           1.0); -- fast time

   begin
      Log_In (Debug, Sideways & " " & Vertical);
      for Index in Speeds'range loop
--       Move_To_Preset (Test, Default_Preset); -- should be set by Set_Up
         Pause (Options.Manual, "watch " & Rate (Index) & " scan " &
           Description);
         Test.Camera_Queue.Process_Command (Command,
            Options     => (
                  (
                     Data           => Speeds (Index),    -- pan speed
                     Start          => 5,
                     Variable_Width => False
                  ),
                  (
                     Data           => Speeds (Index),    -- pan speed
                     Start          => 6,
                     Variable_Width => False
                  )
               ));

         Log_Here (Debug, "wait to send stop");
         delay Wait (Index);
         Log_Here (Debug, "send stop");

         Test.Camera_Queue.Process_Command (Position_Stop,
            Options     => Null_Options);

         delay 0.5;
         Assert (Ask_Pause (Test.Manual,
               "verify that the image shifted " & Description & Rate (Index) &
               " scan for " & Wait (Index)'img &
               " seconds"),
            "manual set failed");
         Move_To_Preset (Test, Default_Preset);
      end loop;
      Log_Out (Debug);
   end Position_Test;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Raw_Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Power'access,
         Routine_Name   => AUnit.Format ("Test_Power")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Update_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Update_Preset")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Reset_Memory'access,
--       Routine_Name   => AUnit.Format ("Test_Reset_Memory")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Move_To_Preset'access,
         Routine_Name   => AUnit.Format ("Test_Move_To_Preset")));

      Log_Out (Debug);
   end Register_Tests;

   ---------------------------------------------------------------
   overriding
   procedure Register_Tests (
      Test                       : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Auto_Focus'access,
--       Routine_Name   => AUnit.Format ("Test_Auto_Focus")));
--
--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Manual_Focus'access,
--       Routine_Name   => AUnit.Format ("Test_Manual_Focus")));
--
--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Position_Absolute'access,
--       Routine_Name   => AUnit.Format ("Test_Position_Absolute")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Down_Left'access,
         Routine_Name   => AUnit.Format ("Test_Position_Down_Left")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Down_Right'access,
         Routine_Name   => AUnit.Format ("Test_Position_Down_Right")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Straight_Down'access,
         Routine_Name   => AUnit.Format ("Test_Position_Straight_Down")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Relative'access,
         Routine_Name   => AUnit.Format ("Test_Position_Relative")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Request'access,
         Routine_Name   => AUnit.Format ("Test_Position_Request")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Right'access,
         Routine_Name   => AUnit.Format ("Test_Position_Right")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Left'access,
         Routine_Name   => AUnit.Format ("Test_Position_Left")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Stop'access,
         Routine_Name   => AUnit.Format ("Test_Position_Stop")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Straight_Up'access,
         Routine_Name   => AUnit.Format ("Test_Position_Straight_Up")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Up_Left'access,
         Routine_Name   => AUnit.Format ("Test_Position_Up_Left")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Up_Right'access,
         Routine_Name   => AUnit.Format ("Test_Position_Up_Right")));

--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Recall_Speed'access,
--       Routine_Name   => AUnit.Format ("Test_Recall_Speed")));
--
--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Zoom_Direct'access,
--       Routine_Name   => AUnit.Format ("Test_Zoom_Direct")));
--
--    Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
--       Routine        => Test_Zoom_Full'access,
--       Routine_Name   => AUnit.Format ("Test_Zoom_Full")));
--
      Log_Out (Debug);
   end Register_Tests;

-- ---------------------------------------------------------------
-- procedure Send_Absolute_Position (
--    Camera                     : in out Base_Camera_Type'class;
--    Pan                        : in     Value_Type;
--    Tilt                       : in     Value_Type) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "Pan " & Pan'img & " tilt " & Tilt'img);
--    Camera.Process_Command (Position_Absolute,
--       Options     => (
--             (
--                Data           => 1,    -- pan slow speed
--                Start          => 5,
--                Variable_Width => False
--             ),
--             (
--                Data           => 1,    -- tlt slow speed
--                Start          => 6,
--                Variable_Width => False
--             ),
--             (
--                Start          => 7,
--                Variable_Width => True,
--                Value          => Pan,
--                Width          => 4
--             ),
--             (
--                Start          => 11,
--                Variable_Width => True,
--                Value          => Tilt,
--                Width          => 4
--             )
--          ));
--    Log_Out (Debug);
-- end Send_Absolute_Position;

-- ---------------------------------------------------------------
-- procedure Set_Absolute (
--    Camera                     : in out Base_Camera_Type'class;
--    Pan                        : in     Absolute_Type;
--    Tilt                       : in     Absolute_Type;
--    Pan_Speed                  : in      Property_Type := 1;
--    Tilt_Speed                 : in      Property_Type := 1) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
--    Camera.Process_Command (Position_Absolute,
--       Options     => (
--          (
--             Data           => Pan_Speed,
--             Start          => 5,
--             Variable_Width => False
--          ),
--          (
--             Data           => Tilt_Speed,
--             Start          => 6,
--             Variable_Width => False
--          ),
--          (
--             Start          => 7,
--             Variable_Width => True,
--             Value          => Convert (Pan),
--             Width          => 4
--          ),
--          (
--             Start          => 11,
--             Variable_Width => True,
--             Value          => Convert (Tilt),
--             Width          => 4
--          )
--       )
--    );
--
--    Log_Out (Debug);
-- end Set_Absolute;

   ---------------------------------------------------------------
   procedure Set_Power (
      Test              : in out Raw_Test_Type'class;
      On                         : in     Boolean) is
   ---------------------------------------------------------------

      Data                       : constant array (Boolean) of Data_Type  := (
                                    False => 3,
                                    True  => 2);
   begin
      Log_In (Debug, "on " & On'img);
      Test.Camera_Queue.Process_Command (Power_Set,
         Options     => ( 1 =>
               (
                  Data           => Data (On),
                  Start          => 5,
                  Variable_Width => False
               )
            ));

         if On then -- need to reopen after power comes on
            Log_Here (Debug);
            Put_Line ("wait 90 seconds for camera to reset");
            for Counter in 1 .. 90 loop
               delay (1.0); -- wait for camera to come back on
               Put (Counter'img & " ");
            end loop;
            New_Line;
            Log_Here (Debug);
            Test.Camera_Queue.Reopen;
         end if;
      Log_Out (Debug);
   end Set_Power;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test              : in out Raw_Test_Type) is
   ---------------------------------------------------------------

      Options        : Standard.Camera.Lib.Unit_Test.
                        Camera_Lib_Unit_Test_Options_Type'class
                           renames Standard.Camera.Lib.Unit_Test.
                              Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
   begin
      Log_In (Debug or Trace_Set_Up);
      Camera.Lib.Unit_Test.Camera_Test_Type (Test).Set_Up;
      Test.Manual := Options.Manual;
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test              : in out Test_Type) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug or Trace_Set_Up);
      Raw_Test_Type (Test).Set_Up;

      declare
         Power_On          : constant Boolean := Get_Power (Test);

      begin
         Log_Here (Debug, "Power_On " & Power_On'img);
         if not Power_On then
            begin
               Set_Power (Test, True);

            exception
               when Fault: others =>
                  Trace_Message_Exception (True, Fault,
                     "ignore exception in Set_Up for Set_Power");
            end;
         end if;
      end;
      Move_To_Preset (Test, Test.Camera_Queue.Get_Default_Preset);
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options        : Standard.Camera.Lib.Unit_Test.
                        Camera_Lib_Unit_Test_Options_Type'class
                           renames Standard.Camera.Lib.Unit_Test.
                              Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Brand          : Brand_Type renames Options.Camera_Options.Brand;
      Test_Suite     : constant AUnit.Test_Suites.Access_Test_Suite :=
                        new AUnit.Test_Suites.Test_Suite;
      Raw_Test       : constant Raw_Test_Access := new Raw_Test_Type (Brand);
      Test           : constant Test_Access := new Test_Type (Brand);
   begin
      Log_In (Debug, "brand " & Brand'img);
      Ada_Lib.Unit_Test.Suite (Raw_Suite_Name);
      Ada_Lib.Unit_Test.Suite (Suite_Name);
      Test_Suite.Add_Test (Raw_Test);
      Test_Suite.Add_Test (Test);
      Log_Out (Debug);
      return Test_Suite;
   end Suite;

-- ---------------------------------------------------------------
-- procedure Test_Auto_Focus (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
-- end Test_Auto_Focus;
--
-- ---------------------------------------------------------------
-- procedure Test_Manual_Focus (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
-- end Test_Manual_Focus;

--   ---------------------------------------------------------------
--   procedure Test_Position_Absolute (
--      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
--   ---------------------------------------------------------------
--
--      Local_Test                 : Test_Type renames Test_Type (Test);
--
--   begin
--      Log_In (Debug);
--      Pause (Local_Test.Manual,
--         "set preset 0 watch for slow scan down for 10 seconds");
--
----    Send_Absolute_Position (Local_Test.Camera_Queue.all, 16#123#, 16#321#);
--      Local_Test.Camera_Queue.Set_Absolute (16#123#, 16#321#);
--      Assert (Ask_Pause (Local_Test.Manual,
--            "verify that the image shifted"),
--         "manual set failed");
--      Log_Out (Debug);
--   end Test_Position_Absolute;

   ---------------------------------------------------------------
   procedure Test_Position_Down_Left (
      Test              : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test        : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Down_Left, "left", "down");
      Log_Out (Debug);
   end Test_Position_Down_Left;

   ---------------------------------------------------------------
   procedure Test_Position_Down_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Down_Right, "right", "down");
   end Test_Position_Down_Right;

   ---------------------------------------------------------------
   procedure Test_Position_Straight_Down (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Down, "", "down");
      Log_Out (Debug);
   end Test_Position_Straight_Down;
   ---------------------------------------------------------------

   ---------------------------------------------------------------
   procedure Test_Position_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Left, "left", "");
      Log_Out (Debug);
   end Test_Position_Left;

   ---------------------------------------------------------------
   procedure Test_Position_Relative (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Initial_Pan                : Absolute_Type;
      Initial_Tilt               : Absolute_Type;
      Local_Test                 : Test_Type renames Test_Type (Test);
      Pan                        : Absolute_Type;
      Tilt                       : Absolute_Type;

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "camera should be at preset 0");
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Initial_Pan, Initial_Tilt);
      Log_Here (Debug, "initial pan" & Initial_Pan'img & " inital tilt" & Initial_Tilt'img);
      for Count in Video.Lib.Relative_Type'(1) .. 10 loop
         declare
            Expected_Pan         : constant Absolute_Type :=
                                    Initial_Pan +
                                    Absolute_Type (Count);
            Expected_Tilt        : constant Absolute_Type :=
                                    Initial_Tilt +
                                    Absolute_Type (Count * 2);

         begin
            Log_Here (Debug, "Count" & Count'img &
               "initial pan" & Initial_Pan'img &
               " initial Tilt" & Initial_Tilt'img);
            Local_Test.Camera_Queue.Position_Relative (
               Pan   => Count,
               Tilt  => Count * 2);
            Local_Test.Camera_Queue.Get_Absolute_Iterate (Pan, Tilt);
            Log_Here (Debug, "pan" & Pan'img & " tilt" & Tilt'img &
               " expected pan" & Expected_Pan'img &
               " expected tilt" & Expected_Tilt'img);
            Assert (abs (Pan - Expected_Pan) <= 1, "bad pan" & Pan'img &
               " expected" & Expected_Pan'img & " for count" & Count'img);
            Assert (abs (Tilt - Expected_Tilt) <= 2, "bad Tilt" & Tilt'img &
               " expected" & Expected_Tilt'img & " for count" & Count'img);
            -- update initial to last read
            Initial_Pan := Pan;
            Initial_Tilt := Tilt;
         end;
      end loop;
      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted up and right"),
         "manual set failed");
      Log_Out (Debug);

   exception

      when Fault: Timed_Out =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);

   end Test_Position_Relative;
   ---------------------------------------------------------------

   ---------------------------------------------------------------
   procedure Test_Position_Request (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);
      Pan                        : Absolute_Type := 0;
--    Response_Buffer            : Maximum_Response_Type;
      Set_Pan                    : constant Absolute_Type := 16#123#;
      Set_Tilt                   : constant Absolute_Type := 16#0bc#;
      Tilt                       : Absolute_Type := 0;
--    Timeout                    : constant Ada_Lib.Time.Time_Type :=
--                                  Ada_Lib.Time.Now + 60.0;
   begin
      Log_In (Debug, "set pan " & Set_Pan'img & " set tilt " & Set_Tilt'img);
      Local_Test.Camera_Queue.Set_Absolute (Set_Pan, Set_Tilt);
      Local_Test.Camera_Queue.Get_Absolute_Iterate (Pan, Tilt);

      Assert (Pan = Set_Pan, "invalid pan " & Pan'img &
         " expected " & Set_Pan'img);
      Assert (Tilt = Set_Tilt, "invalid tilt " & Tilt'img &
         " expected " & Set_Tilt'img);
      Log_Out (Debug, "Pan " & Pan'img & " tilt " & Tilt'img);
   end Test_Position_Request;

   ---------------------------------------------------------------
   procedure Test_Position_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Right, "right", "");
      Log_Out (Debug);
   end Test_Position_Right;

   ---------------------------------------------------------------
   procedure Test_Position_Stop (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Local_Test.Camera_Queue.Process_Command (Position_Stop,
         Options     => (
               (
                  Data           => 1,    -- pan slow speed
                  Start          => 5,
                  Variable_Width => False
               ),
               (
                  Data           => 1,    -- tlt slow speed
                  Start          => 6,
                  Variable_Width => False
               )
            ));
      Log_Out (Debug);
   end Test_Position_Stop;

   ---------------------------------------------------------------
   procedure Test_Position_Straight_Up (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Up, "", "up");
      Log_Out (Debug);
   end Test_Position_Straight_Up;

   ---------------------------------------------------------------
   procedure Test_Position_Up_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Up_Left, "left", "up");
      Log_Out (Debug);
   end Test_Position_Up_Left;

   ---------------------------------------------------------------
   procedure Test_Position_Up_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Position_Test (Local_Test, Position_Up_Right, "right", "up");
      Log_Out (Debug);
   end Test_Position_Up_Right;

   ---------------------------------------------------------------
   procedure Test_Power (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Raw_Test_Type renames Raw_Test_Type (Test);

   begin
      Log_In (Debug);
      for On in Boolean'range loop
         Log_Here (Debug, "on " & On'img);
         Set_Power (Local_Test, On);
         declare
            Power                : constant Boolean :=
                                    Get_Power (Local_Test);
         begin
            Log_Here (Debug, "Power " & Power'img);
            Pause_On_Flag ("power " & Power'img & " got");
            Assert (Power = On, "power not set to " & On'img);
         end;
      end loop;
      Log_Out (Debug);
   end Test_Power;

   ---------------------------------------------------------------
   procedure Test_Update_Preset (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Raw_Test_Type renames Raw_Test_Type (Test);
      Default_Preset             : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Get_Default_Preset;
      Minimum_Test_Preset        : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Minimum_Test_Preset;
      Test_Preset                : constant  Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Last_Preset;
      Alternate_Preset           : constant Preset_ID_Type := Default_Preset + 1;

   begin
      Log_In (Debug, "Test_Preset" & Test_Preset'img &
         " saved preset" & Default_Preset'img &
         " Minimum_Test_Preset" & Minimum_Test_Preset'img);
      if Test_Preset < Minimum_Test_Preset then
         raise Failed with "tried to set non testing preset id" &
            Test_Preset'img & ". Less than minimum for testing" &
            Minimum_Test_Preset'img;
      end if;
      Local_Test.Move_To_Preset (Default_Preset);
      delay 2.5;
      if Local_Test.Manual then
         Assert (Ask_Pause (True, "check default preset" & Default_Preset'img),
            "move to preset" & Default_Preset'img & " failed");
      else
         Put_Line ("camera moved to default preset" & Default_Preset'img);
      end if;
      Local_Test.Camera_Queue.Process_Command (Memory_Set,
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Test_Preset),
                  Start          => 6,
                  Variable_Width => False
               )
            ));
      delay 3.0;  -- camera needs time to update memory
      Put_Line ("preset" & Test_Preset'img &
         " saved location" & Default_Preset'img);
      Local_Test.Move_To_Preset (Alternate_Preset);
      if Local_Test.Manual then
         Assert (Ask_Pause (True, "check alternate preset" & Alternate_Preset'img),
            "move to preset" & Alternate_Preset'img & " failed");
      else
         delay 2.5;  -- wait for camera to move
         Put_Line ("camera moved to alternate preset" & Alternate_Preset'img);
      end if;
      Local_Test.Move_To_Preset (Test_Preset);
      delay 2.5;
      if Local_Test.Manual then
         Assert (Ask_Pause (True, "check test preset" & Test_Preset'img &
            ". Should be same as" & Default_Preset'img),
            "move to preset" & Test_Preset'img & " failed");
      else
         Put_Line ("camera moved to test preset" & Test_Preset'img);
      end if;

      Log_Out (Debug);
   end Test_Update_Preset;

-- ---------------------------------------------------------------
-- -- reset does not seem to change camera setting
-- procedure Test_Reset_Memory (
--    Test                 : in out AUnit.Test_Cases.Test_Case'class) is
-- ---------------------------------------------------------------
--
--    Local_Test        : Raw_Test_Type renames Raw_Test_Type (Test);
--    Default_Preset    : constant Preset_ID_Type :=
--                         Local_Test.Camera_Queue.Get_Default_Preset;
--    Options           : Standard.Camera.Lib.Unit_Test.
--                         Camera_Lib_Unit_Test_Options_Type'class
--                            renames Standard.Camera.Lib.Unit_Test.
--                               Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
--    Response          : Maximum_Response_Type;
--    Response_Length   : Index_Type;
--    Test_Preset       : constant Preset_ID_Type := Local_Test.Camera_Queue.Last_Preset;
--    Different_Preset  : constant Preset_ID_Type := Default_Preset + 1;
--
-- begin
--    Log_In (Debug, "set initial preset");
--    -- set camera to Default_Preset
--    Local_Test.Camera_Queue.Process_Command (Memory_Recall,
--       Options           => ( 1 =>
--             (
--                Data           => Data_Type (Default_Preset),
--                Start          => 6,
--                Variable_Width => False
--             )
--          ));
--    delay 2.0;  -- camera needs time to mobr
--    Log_Here (Debug, "update test preset");
--    Local_Test.Camera_Queue.Process_Command (Memory_Set,
--       Options     => ( 1 =>
--             (
--                Data           => Data_Type (Test_Preset),
--                Start          => 6,
--                Variable_Width => False
--             )
--          ));
--    delay 3.0;  -- camera needs time to update memory
--    Log_Here (Debug, "move to alterante preset");
--    Local_Test.Camera_Queue.Process_Command (Memory_Recall,
--       Options           => ( 1 =>
--             (
--                Data           => Data_Type (Different_Preset),
--                Start          => 6,
--                Variable_Width => False
--             )
--          ));
--    delay 2.0;  -- camera needs time to mobr
--    Local_Test.Camera_Queue.Process_Command (Memory_Reset,
--       Options           => ( 1 =>
--             (
--                Data           => Data_Type (Test_Preset),
--                Start          => 6,
--                Variable_Width => False
--             )
--          ),
--       Response          => Response,
--       Response_Length   => Response_Length);
--    Log_Here (Debug, "set camera to different preset");
--    -- set camera to Different_Preset
--    Local_Test.Camera_Queue.Move_To_Preset (Different_Preset);
--    Pause (Options.Manual, "at different preset " & Different_Preset'img);
--    Log_Here (Debug, "set camera to test preset");
--    Local_Test.Camera_Queue.Move_To_Preset (Test_Preset);
--
--    Pause (Options.Manual, "at test preset " & Test_Preset'img);
--    Assert (Ask_Pause (Local_Test.Manual,
--          "verify camera at preset" & Default_Preset'img),
--       "manual set failed");
--    Log_Out (Debug);
-- end Test_Reset_Memory;

   ---------------------------------------------------------------
   procedure Test_Move_To_Preset (         -- tests Memory_Set - sets camera to preset
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Raw_Test_Type renames Raw_Test_Type (Test);
      Default_Preset             : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Get_Default_Preset;
      Test_Preset                : constant Preset_ID_Type :=
                                    Local_Test.Camera_Queue.Last_Preset;

   begin
      Log_In (Debug, "Test_Preset" & Test_Preset'img);
      Pause (Local_Test.Manual, "set preset" & Test_Preset'img);
      Local_Test.Move_To_Preset (Default_Preset);
      if Local_Test.Manual then
         Assert (Ask_Pause (True, "check default preset" & Default_Preset'img),
            "move to preset" & Default_Preset'img & " failed");
      end if;
      Local_Test.Move_To_Preset (Test_Preset);

      Assert (Ask_Pause (Local_Test.Manual,
         "verify that the preset" & Test_Preset'img),
         "manual set failed");
      Log_Out (Debug);

   exception
      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);


   end Test_Move_To_Preset;

-- ---------------------------------------------------------------
-- procedure Test_Recall_Speed (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
-- end Test_Recall_Speed;
--
-- ---------------------------------------------------------------
-- procedure Test_Zoom_Direct (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
-- end Test_Zoom_Direct;
--
-- ---------------------------------------------------------------
-- procedure Test_Zoom_Full (
--    Test                       : in out AUnit.Test_Cases.Test_Case'class) is
-- pragma Unreferenced (Test);
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug);
--    Log_Out (Debug);
-- end Test_Zoom_Full;

   ---------------------------------------------------------------
   procedure Wait (
      Length                     : in     Duration) is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
   begin
      if not Options.Camera_Options.If_Emulation then
         delay Length;
      end if;
   end Wait;

 begin
--Debug := True;
    Log (Debug, Here, Who);
 end Camera.Lib.Base.Command_Tests;
