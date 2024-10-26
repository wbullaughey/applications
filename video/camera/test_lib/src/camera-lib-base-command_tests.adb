--with Ada.Streams;
--with Ada_Lib.GNOGA;
--with Ada_Lib.Time;
with Ada_Lib.Unit_Test;
with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases;
--with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Camera.Lib.Base;
--with Camera.Commands;
--with Camera.Lib.Connection;
with Camera.Lib.Unit_Test;
with Interfaces;
with Video.Lib;

package body Camera.Lib.Base.Command_Tests is

-- use type Ada.Streams.Stream_Element;
-- use type Ada_Lib.Time.Time_Type;
   use type Interfaces.Integer_16;
-- use type Interfaces.Unsigned_16;

   type Test_Type (
      Brand                      : Brand_Type;
      Description                : Ada_Lib.Strings.String_Constant_Access
                                    ) is new Camera.Lib.Unit_Test.
                                       Camera_Test_Type (Brand, Description
                                          ) with record
      Manual                     : Boolean := False;
   end record;

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

   procedure Test_Position_Down (
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

   procedure Test_Position_Up (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Up_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Position_Up_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Power (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Recall_Memory (
      Test                       : in out AUnit.Test_Cases.Test_Case'class);

   procedure Test_Recall_Set (
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
         Routine        => Test_Position_Down'access,
         Routine_Name   => AUnit.Format ("Test_Position_Down")));

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
         Routine        => Test_Position_Up'access,
         Routine_Name   => AUnit.Format ("Test_Position_Up")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Up_Left'access,
         Routine_Name   => AUnit.Format ("Test_Position_Up_Left")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Position_Up_Right'access,
         Routine_Name   => AUnit.Format ("Test_Position_Up_Right")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Power'access,
         Routine_Name   => AUnit.Format ("Test_Power")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Recall_Memory'access,
         Routine_Name   => AUnit.Format ("Test_Recall_Memory")));

      Test.Add_Routine (AUnit.Test_Cases.Routine_Spec'(
         Routine        => Test_Recall_Set'access,
         Routine_Name   => AUnit.Format ("Test_Recall_Set")));

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
      Camera                     : in out Base_Camera_Type'class;
      On                         : in     Boolean) is
   ---------------------------------------------------------------

      Data                       : constant array (Boolean) of Data_Type  := (
                                    False => 3,
                                    True  => 2);
   begin
      Log_In (Debug, "on " & On'img);
      Camera.Process_Command (Power,
         Options     => ( 1 =>
               (
                  Data           => Data (On),
                  Start          => 5,
                  Variable_Width => False
               )
            ));

      Log_Out (Debug);
   end Set_Power;

-- ---------------------------------------------------------------
-- procedure Set_Preset (
--    Camera                     : in out Base_Camera_Type'class;
--    Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
--    Wait_Until_Finished        : in     Boolean := True) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "preset id" & Preset_ID'img);
--    Camera.Process_Command (Memory_Recall,
--       Options     => ( 1 =>
--             (
--                Data           => Data_Type (Preset_ID),
--                Start          => 6,
--                Variable_Width => False
--             )
--          ));
--
--    if Wait_Until_Finished then
--       declare
--          Pan                  : Absolute_Type;
--          Tilt                 : Absolute_Type;
--       begin
--          Get_Absolute (Camera, Pan, Tilt);
--          Log_Here (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
--       end;
--    end if;
--    Log_Out (Debug);
-- end Set_Preset;

   ---------------------------------------------------------------
   overriding
   procedure Set_Up (
      Test              : in out Test_Type) is
   ---------------------------------------------------------------

--    Connection_Data   : constant Connection.Connection_Data_Access :=
--                         Connection.Connection_Data_Access (
--                            Ada_Lib.GNOGA.Get_Connection_Data);
   begin
      Log_In (Debug or Trace_Set_Up);
--    Connection_Data.Initialize;
      Camera.Lib.Unit_Test.Camera_Test_Type (Test).Set_Up;

      begin
         Set_Power (Test.Camera.all, True);
      exception
         when Fault: others =>
            Trace_Message_Exception (True, Fault,
               "ignore exception in Set_Up for Set_Power");
      end;
      Test.Camera.Set_Preset (Test.Camera.Get_Default_Preset);
      Log_Out (Debug or Trace_Set_Up);
   end Set_Up;

   ---------------------------------------------------------------
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   ---------------------------------------------------------------

      Options                    : Standard.Camera.Lib.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type'class
                                       renames Standard.Camera.Lib.Unit_Test.
                                          Get_Camera_Lib_Unit_Test_Read_Only_Options.all;
      Brand                      : Brand_Type renames Options.Camera_Options.Brand;
      Test_Suite                 : constant AUnit.Test_Suites.Access_Test_Suite :=
                                    new AUnit.Test_Suites.Test_Suite;
      Test                       : constant Test_Access := new Test_Type (Brand,
                                    new String'("camera"));

   begin
      Log_In (Debug, "brand " & Brand'img);
      Ada_Lib.Unit_Test.Suite (Suite_Name);
      Test_Suite.Add_Test (Test);
      Test.Set_Camera;
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
----    Send_Absolute_Position (Local_Test.Camera.all, 16#123#, 16#321#);
--      Local_Test.Camera.Set_Absolute (16#123#, 16#321#);
--      Assert (Ask_Pause (Local_Test.Manual,
--            "verify that the image shifted"),
--         "manual set failed");
--      Log_Out (Debug);
--   end Test_Position_Absolute;

   ---------------------------------------------------------------
   procedure Test_Position_Down_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual,
         "set preset 0 watch for slow scan down left for 10 seconds");

      Local_Test.Camera.Process_Command (Position_Down_Left,
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

      Log_Here (Debug, "wit to send stop");
      Wait (2.0);
      Log_Here (Debug, "send stop");

      Local_Test.Camera.Process_Command (Position_Stop,
         Options     => Null_Options);

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted down left slowly, watch for fast scan for 5 seconds"),
         "manual set failed");
      Local_Test.Camera.Process_Command (Position_Down_Left,
         Options     => (
               (
                  Data           => 16#18#,    -- pan high speed
                  Start          => 5,
                  Variable_Width => False
               ),
               (
                  Data           => 16#14#,    -- tilt hight speed
                  Start          => 6,
                  Variable_Width => False
               )
            ));

      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
         Options     => Null_Options);

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted down left fast"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Down_Left;

   ---------------------------------------------------------------
   procedure Test_Position_Down_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "set preset 0");

      Local_Test.Camera.Process_Command (Position_Down_Right,
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
      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
         Options     => Null_Options);

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted down right"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Down_Right;

   ---------------------------------------------------------------
   procedure Test_Position_Down (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual,
         "set preset 0 watch for slow scan down for 10 seconds");

      Local_Test.Camera.Process_Command (Position_Down,
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

      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted down slowly, watch for fast scan for 5 seconds"),
         "manual set failed");
      Local_Test.Camera.Process_Command (Position_Down,
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

      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted down fast"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Down;
   ---------------------------------------------------------------

   ---------------------------------------------------------------
   procedure Test_Position_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual,
         "set preset 0 watch for slow scan left for 10 seconds");

      Local_Test.Camera.Process_Command (Position_Left,
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
      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted left slowly, watch for fast scan for 5 seconds"),
         "manual set failed");
      Local_Test.Camera.Process_Command (Position_Left,
         Options     => (
               (
                  Data           => 16#0C#,    -- pan slow speed
                  Start          => 5,
                  Variable_Width => False
               ),
               (
                  Data           => 16#10#,    -- tlt slow speed
                  Start          => 6,
                  Variable_Width => False
               )
            ));
      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted left fast"),
         "manual set failed");
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
      Get_Absolute (Local_Test.Camera.all, Initial_Pan, Initial_Tilt);
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
            Local_Test.Camera.Position_Relative (
               Pan   => Count,
               Tilt  => Count * 2);
            Local_Test.Camera.Get_Absolute (Pan, Tilt);
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
      Local_Test.Camera.Set_Absolute (Set_Pan, Set_Tilt);
      Local_Test.Camera.Get_Absolute (Pan, Tilt);

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
      Pause (Local_Test.Manual,
         "set preset 0 watch for slow scan right for 10 seconds");

      Local_Test.Camera.Process_Command (Position_Right,
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
      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted right slowly, watch for fast scan for 5 seconds"),
         "manual set failed");
      Local_Test.Camera.Process_Command (Position_Right,
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
      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted right fast"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Right;

   ---------------------------------------------------------------
   procedure Test_Position_Stop (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Local_Test.Camera.Process_Command (Position_Stop,
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
   procedure Test_Position_Up (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual,
         "set preset 0 watch for slow scan up for 10 seconds");

      Local_Test.Camera.Process_Command (Position_Up,
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

      Wait (2.0);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted up slowly, watch for fast scan for 5 seconds"),
         "manual set failed");
      Local_Test.Camera.Process_Command (Position_Up,
         Options     => (
               (
                  Data           => 16#0C#,    -- pan high speed
                  Start          => 5,
                  Variable_Width => False
               ),
               (
                  Data           => 16#10#,    -- tilt high speed
                  Start          => 6,
                  Variable_Width => False
               )
            ));
      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted up fast"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Up;

   ---------------------------------------------------------------
   procedure Test_Position_Up_Left (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "set preset 0");

      Local_Test.Camera.Process_Command (Position_Up_Left,
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

      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted up left"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Up_Left;

   ---------------------------------------------------------------
   procedure Test_Position_Up_Right (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "set preset 0");

      Local_Test.Camera.Process_Command (Position_Up_Right,
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
      Wait (1.5);

      Local_Test.Camera.Process_Command (Position_Stop,
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

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the image shifted up right"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Position_Up_Right;

   ---------------------------------------------------------------
   procedure Test_Power (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      for On in Boolean'range loop
         Set_Power (Local_Test.Camera.all, On);
      end loop;
      Log_Out (Debug);
   end Test_Power;

   ---------------------------------------------------------------
   procedure Test_Recall_Memory (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "set preset 3");
      Local_Test.Camera.Set_Preset (3);

      Assert (Ask_Pause (Local_Test.Manual,
            "verify that the preset is 3"),
         "manual set failed");
      Log_Out (Debug);

   exception
      when Fault: others =>
         Ada_Lib.Unit_Test.Exception_Assert (Fault);


   end Test_Recall_Memory;

   ---------------------------------------------------------------
   procedure Test_Recall_Set (
      Test                       : in out AUnit.Test_Cases.Test_Case'class) is
   ---------------------------------------------------------------

      Local_Test                 : Test_Type renames Test_Type (Test);
      Test_Preset                : constant := 254;

   begin
      Log_In (Debug);
      Pause (Local_Test.Manual, "possition camera away from preset");

      Local_Test.Camera.Process_Command (Memory_Set,
         Options     => ( 1 =>
               (
                  Data           => Test_Preset,
                  Start          => 6,
                  Variable_Width => False
               )
            ));

      Local_Test.Camera.Process_Command (Memory_Recall,
         Options     => ( 1 =>
               (
                  Data           => 0,   -- preset 0
                  Start          => 6,
                  Variable_Width => False
               )
            ));

      Assert (Ask_Pause (Local_Test.Manual,
            "verify camer at preset 0"),
         "manual set failed");
      Log_Out (Debug);
   end Test_Recall_Set;

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
