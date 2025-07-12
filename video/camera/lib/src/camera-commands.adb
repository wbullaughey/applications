with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Camera.Lib;
with Hex_IO;

package body Camera.Commands is

   use type Ada.Streams.Stream_Element;
   use type Ada_Lib.Time.Time_Type;
   use type Index_Type;
   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;
-- use type Value_Type;

   Delay_After_Move     : constant := 0.5;
   Power_On_Delay       : constant := 120.0;
   Zoom_Stabalize_Time  : constant := 15.0;

   ------------------------------------------------------------
   function Accumulate (
      Description             : in     String;
      Response_Buffer         : in     Maximum_Response_Type;
      Start                   : in     Index_Type
   ) return Absolute_Type is
   ------------------------------------------------------------

      Accumulator          : Interfaces.Unsigned_16;
      Conversion           : Absolute_Type;
      for Conversion'address use Accumulator'address;

   begin
      for I in Start .. Start + 3 loop
         Log_Here (Debug, I'img & ": " &
            Ada_lib.Socket_IO.Hex (Response_Buffer (I)));
         Accumulator := Accumulator * 16#10# +
            Interfaces.Unsigned_16 (Response_Buffer (I) and 16#F#);
      end loop;

      Log_Here (Debug, Description & " " & Hex_IO.Hex (Accumulator) &
         " conversion" & Conversion'img);

      return Conversion;
   end Accumulate;

   ---------------------------------------------------------------
   function Convert (
      Relative                   : in     Relative_Type
   ) return Value_Type is
   ---------------------------------------------------------------

      Conversion                 : Interfaces.Unsigned_16;
      for Conversion'address use Relative'address;
      Result                     : constant Value_Type :=
                                    Value_Type (Conversion);

   begin
      Log_Here (Debug, "Relative" & Relative'img & " result" & Result'img);
      return Result;
   end Convert;

   ---------------------------------------------------------------
   function Convert (
      Absolute                   : in     Absolute_Type
   ) return Value_Type is
   ---------------------------------------------------------------

      Conversion                 : Interfaces.Unsigned_16;
      for Conversion'address use Absolute'address;
      Result                     : constant Value_Type :=
                                    Value_Type (Conversion);

   begin
      Log_Here (Debug, "absolute" & Absolute'img & " result" & Result'img);
      return Result;
   end Convert;

   ---------------------------------------------------------------
   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      Stabalize_Time             : in     Ada_Lib.Time.Duration_Type :=
                                             Wait_Until_Finished_Time) is
   ---------------------------------------------------------------

      Last_Pan       : Absolute_Type := Absolute_Type'last;
      Last_Tilt      : Absolute_Type := Absolute_Type'last;
      Timeout_Time   : constant Ada_Lib.Time.Time_Type := Ada_Lib.Time.Now +
                        Stabalize_Time;
      Tries          : Natural := 0;

   begin
log_here (Test_Condition, from_start (Timeout_Time));
      Log_In (Debug, "timeout time " & From_Start (Timeout_Time));
      loop
         declare
            Response_Buffer      : Maximum_Response_Type;

         begin
            Camera.Process_Command (Standard.Camera.Lib.Base.Position_Request,
               Options           => Standard.Camera.Lib.Base.Null_Option,
               Response          => Response_Buffer);

            if Debug then
               Ada_Lib.Socket_IO.Stream_IO.Dump ("response", Response_Buffer (1 .. 11));
            end if;

            Pan := Accumulate ("Pan", Response_Buffer, 3);
            Tilt := Accumulate ("Tilt", Response_Buffer, 7);

            if Last_Pan /= Absolute_Type'last then
               Tries := Tries + 1;
               declare
                  Delta_Pan         : constant Integer := abs (
                                       Integer (Last_Pan) - Integer (Pan));
                  Delta_Tilt        : constant Integer := abs (
                                       Integer (Last_Tilt) - Integer (Tilt));
                  Delta_Message     : constant String := "try" & Tries'img &
                                       " delta pan " & Delta_Pan'img &
                                       " delta tilt " & Delta_Tilt'img;

               begin
                  Log_Here (Debug, "Pan " & Pan'img & " Tilt " & Tilt'img &
                     " " & Delta_Message);
                  if    Delta_Pan = 0 and then
                        Delta_Tilt = 0 then
                     Log_Out (Debug);
                     return;
                  end if;
               end;
            end if;

            Last_Pan := Pan;
            Last_Tilt := Tilt;
         end;

         delay 0.1;

         declare
            Now      : constant Ada_Lib.Time.Time_Type := Ada_Lib.Time.Now;

         begin
            Log_Here (Debug, "now " & From_Start (Now));
            if Ada_Lib.Time.Now > Timeout_Time then
               declare
                  Message  : constant String :=
                     "timeout waiting for stable aboslute location " &
                     " in Get_Absolute " & Here &
                     " timeout " & Stabalize_Time'img;
               begin
                  Log_Exception (Debug, Message);
                  raise Timeout with Message;
               end;
            end if;
         end;
      end loop;

   exception

      when Fault: Timeout =>
         Log_Exception (Debug, Fault,
            "timeout waiting for absolute position to stablize");
         raise;

   end Get_Absolute;

   ---------------------------------------------------------------
   function Get_Power (
      Camera                     : in out Camera_Type
   ) return Boolean is
   ---------------------------------------------------------------

      Response_Buffer      : Maximum_Response_Type;

   begin
      Log_In (Debug);
      Camera.Process_Command (Standard.Camera.Lib.Base.Power_Inquire,
         Options           => Standard.Camera.Lib.Base.Null_Option,
         Response          => Response_Buffer);

      if Debug then
         Video.Lib.Dump ("response", Response_Buffer,
            Positive (Camera_Type'class (Camera).Get_Ack_Length));
      end if;

      case Response_Buffer (3) is

         when 2 =>
            return Log_Out (True, Debug);

         when 3 =>
            return Log_Out (False, Debug);

         when 4 =>
            Put_Line ("circuit error");
            return Log_Out (False, Debug);

         when others =>
            Put_Line ("invalid power inquire code " &
               Ada_lib.Socket_IO.Hex (Response_Buffer (3)));
            return Log_Out (False, Debug);

      end case;
   end Get_Power;

   ---------------------------------------------------------------
   procedure Get_Zoom (
      Camera                     : in out Camera_Type;
      Zoom                       :    out Absolute_Type) is
   ---------------------------------------------------------------

      Last_Zoom      : Absolute_Type := Absolute_Type'last;
      Timeout_Time   : constant Ada_Lib.Time.Time_Type := Ada_Lib.Time.Now +
                        Zoom_Stabalize_Time;

   begin
      Log_In (Debug);
      loop
         if Ada_Lib.Time.Now > Timeout_Time then
            raise Timeout with "timeout waiting for stable zoom";
         end if;

         declare
            Response_Buffer      : Maximum_Response_Type;
            Timeout              : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + 60.0;
         begin
            Camera.Process_Command (Standard.Camera.Lib.Base.Zoom_Inquire,
               Options           => Standard.Camera.Lib.Base.Null_Option,
               Response          => Response_Buffer);

            if Debug then
               Ada_Lib.Socket_IO.Stream_IO.Dump ("response", Response_Buffer (1 .. 11));
            end if;

            Zoom := Accumulate ("Zoom", Response_Buffer, 3);

            if Last_Zoom /= Absolute_Type'last then
               declare
                  Delta_Zoom            : constant Integer := abs (
                                          Integer (Last_Zoom) - Integer (Zoom));
                  Delta_Message        : constant String :=
                                          " delta zoom" & Delta_Zoom'img;
               begin
                  Log_Here (Debug, "Zoom" & Zoom'img & Delta_Message);
                  if    Delta_Zoom < 2 then
                     Log_Out (Debug);
                     return;
                  elsif Ada_Lib.Time.Now > Timeout then
                     declare
                        Message        : constant String :=
                                          "Get_Absolute did not converge. " &
                                          Delta_Message;
                     begin
                        Log_Exception (Debug, Message);
                        raise Failed with Message;
                     end;
                  end if;
               end;
            end if;

            Last_Zoom := Zoom;
         end;
      end loop;
   end Get_Zoom;

   ---------------------------------------------------------------
   procedure Position_Relative (
      Camera                     : in out Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Wait_For_Complete          : in      Boolean := True;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is
   ---------------------------------------------------------------

   begin
Log_In (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
      Camera.Process_Command (Standard.Camera.Lib.Base.Position_Relative,
         Options     => (
            (
               Data           => Pan_Speed,
               Mode           => Standard.Camera.Lib.Base.Fixed,
               Start          => 5
            ),
            (
               Data           => Tilt_Speed,
               Mode           => Standard.Camera.Lib.Base.Fixed,
               Start          => 6
            ),
            (
               Mode           => Standard.Camera.Lib.Base.Variable,
               Start          => 7,
               Value          => Convert (Pan),
               Width          => 4
            ),
            (
               Mode           => Standard.Camera.Lib.Base.Variable,
               Start          => 11,
               Value          => Convert (Tilt),
               Width          => 4
            )
         )
      );

      if Wait_For_Complete then
         delay Delay_After_Move;
         declare
            Pan               : Absolute_Type;
            Tilt              : Absolute_Type;

         begin
            Camera.Get_Absolute (Pan, Tilt);
         end;
      end if;

Log_Out (Debug);
Pause_On_Flag ("exit Position_Relative", Here, TRue);
   end Position_Relative;

   ---------------------------------------------------------------
   procedure Set_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
      Camera.Process_Command (Standard.Camera.Lib.Base.Position_Absolute,
         Options     => (
            (
               Data           => Pan_Speed,
               Start          => 5,
               Mode           => Standard.Camera.Lib.Base.Fixed
            ),
            (
               Data           => Tilt_Speed,
               Start          => 6,
               Mode           => Standard.Camera.Lib.Base.Fixed
            ),
            (
               Start          => 7,
               Mode           => Standard.Camera.Lib.Base.Variable,
               Value          => Convert (Pan),
               Width          => 4
            ),
            (
               Start          => 11,
               Mode           => Standard.Camera.Lib.Base.Variable,
               Value          => Convert (Tilt),
               Width          => 4
            )
         )
      );

      if Wait_For_Complete then
         delay Delay_After_Move;
         declare
            Pan               : Absolute_Type;
            Tilt              : Absolute_Type;

         begin
            Camera.Get_Absolute (Pan, Tilt);
         end;
      end if;

      Log_Out (Debug);
   end Set_Absolute;

   ---------------------------------------------------------------
   procedure Set_Direct_Zoom (
      Camera                  : in out Camera_Type;
      Value                   : in     Absolute_Type;
      Wait_For_Complete       : in     Boolean := True) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "Zoom" & Value'img);
      Camera.Process_Command (Standard.Camera.Lib.Base.Zoom_Direct,
         Options     => (
            1  => (
                  Start          => 5,
                  Mode           => Standard.Camera.Lib.Base.Variable,
                  Value          => Convert (Value),
                  Width          => 4
               )
         )
      );

      if Wait_For_Complete then
         delay Delay_After_Move;
         declare
            Zoom              : Absolute_Type;

         begin
            Camera.Get_Zoom (Zoom);
         end;
      end if;

      Log_Out (Debug);
   end Set_Direct_Zoom;

   ---------------------------------------------------------------
   procedure Set_Fixed_Zoom (
      Camera                     : in out Camera_Type;
      Mode                       : in     Zoom_Mode_Type;
      Wait_For_Complete          : in     Boolean := True) is
   ---------------------------------------------------------------

   Command  : constant array (Zoom_Mode_Type) of
               Standard.Camera.Lib.Base.Commands_Type := (
                  Standard.Camera.Lib.Base.Zoom_Tele_Standard,
                  Standard.Camera.Lib.Base.Zoom_Wide_Standard);

   begin
      Log_In (Debug, "mode " & Mode'img &
         " Wait_For_Complete " & Wait_For_Complete'img);
      Camera.Process_Command (Command (Mode),
         Options           => Standard.Camera.Lib.Base.Null_Option);

      if Wait_For_Complete then
         delay Delay_After_Move;
         declare
            Zoom              : Absolute_Type;

         begin
            Camera.Get_Zoom (Zoom);
         end;
      end if;

      Log_Out (Debug);
   end Set_Fixed_Zoom;

   ---------------------------------------------------------------
   procedure Set_Power (
      Camera            : in out Camera_Type;
      On                : in     Boolean) is
   ---------------------------------------------------------------

      Data              : constant array (Boolean) of Data_Type  := (
                           False => 3,
                           True  => 2);

   begin
      Log_In (Debug, "turn on " & On'img);
      declare
         Current_State  : constant Boolean := Camera.Get_Power;

      begin
         Log_Here (Debug, "current state " & Current_State'img);
         if On = Current_State then    -- no change
            Log_Out (Debug, "no change. current state " & Current_State'img &
               " new state " & On'img);
            return;
         end if;

         -- change state

         case On is

            when False  => -- turning power off
               -- normal command processing
               Camera.Process_Command (Standard.Camera.Lib.Base.Power,
                  Options     => ( 1 =>
                        (
                           Data           => Data (On),
                           Start          => 5,
                           Mode           => Standard.Camera.Lib.Base.Fixed
                        )
                     ));

            when True  => -- turning power on
               declare
                  Get_Ack                    :  Boolean;
                  Has_Response               :  Boolean;
                  Response_Length            :  Index_Type;
                  Response                   :  Maximum_Response_Type;

               begin
                  Camera_Type'class (Camera).Send_Command (
                     Standard.Camera.Lib.Base.Power,
                     Get_Ack, Has_Response, Response_Length);
                  Log_Here (Debug, "Get_Ack " & Get_Ack'img &
                     " Has_Response " & Has_Response'img &
                     " Response_Length " & Response_Length'img &
                     " delay for " & Power_On_Delay'img & " seconds");

                  delay Power_On_Delay;

                  Camera_Type'class (Camera).Get_Response (Get_Ack,
                     Has_Response, Response, Response_Length,
                     Camera_Type'class (Camera).Get_Timeout (
                        Standard.Camera.Lib.Base.Power));
               end;

         end case;

         declare
            Power    : constant Boolean := Camera.Get_Power;
            Message  : constant String := "power not set to " & On'img;

         begin
            if Power /= On then
               Log_Exception (Debug, Message);
               raise Failed with Message;
            end if;

            Log_Out (Debug, "power " & Power'img);
         end;
      end;
   end Set_Power;

   ---------------------------------------------------------------
   procedure Set_Preset (
      Camera                     : in out Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img &
         " wait " & Wait_Until_Finished'img);
      Camera.Process_Command (Standard.Camera.Lib.Base.Memory_Recall,
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Preset_ID),
                  Start          => 6,
                  Mode           => Standard.Camera.Lib.Base.Fixed
               )
            ));

      if Wait_Until_Finished then
         delay Delay_After_Move;
         declare
            Pan                  : Absolute_Type;
            Tilt                 : Absolute_Type;
         begin
            Camera.Get_Absolute (Pan, Tilt);
            Log_Here (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
         end;
      end if;
      Log_Out (Debug);

   exception

      when Fault: others =>
         Log_Exception (Debug, Fault, "timeout in Set_Preset Read");
         raise;

   end Set_Preset;

   ---------------------------------------------------------------
   procedure Set_Variable_Zoom (
      Camera                     : in out Camera_Type;
      Mode                       : in     Zoom_Mode_Type;
      Value                      : in     Absolute_Type;
      Wait_For_Complete          : in     Boolean := True) is
   ---------------------------------------------------------------

   Command  : constant array (Zoom_Mode_Type) of
               Standard.Camera.Lib.Base.Commands_Type := (
                  Standard.Camera.Lib.Base.Zoom_Tele_Variable,
                  Standard.Camera.Lib.Base.Zoom_Wide_Variable);

   begin
      Log_In (Debug, "Zoom" & Value'img);
      Camera.Process_Command (Command (Mode),
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Value),
                  Start          => 5,
                  Mode           => Standard.Camera.Lib.Base.Fixed
               )
            ));

      if Wait_For_Complete then
         delay Delay_After_Move;
         declare
            Zoom              : Absolute_Type;

         begin
            Camera.Get_Zoom (Zoom);
         end;
      end if;

      Log_Out (Debug);
   end Set_Variable_Zoom;
   ---------------------------------------------------------------

begin
--Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Camera.Commands;
