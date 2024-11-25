with Ada.Streams;
with Ada_Lib.Time;
with ADA_LIB.Trace; use ADA_LIB.Trace;
--with Camera_Queue.Lib;
--with Camera.Lib.Base;
with Hex_IO;
with Interfaces;

package body Camera.Commands is

   use type Ada.Streams.Stream_Element;
   use type Ada_Lib.Time.Time_Type;
   use type Index_Type;
   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;
-- use type Value_Type;

-- ---------------------------------------------------------------
-- function Convert (
--    Relative                   : in     Relative_Type
-- ) return Value_Type is
-- ---------------------------------------------------------------
--
--    Conversion                 : Interfaces.Unsigned_16;
--    for Conversion'address use Relative'address;
--    Result                     : constant Value_Type :=
--                                  Value_Type (Conversion);
--
-- begin
--    Log_Here (Debug, "Relative" & Relative'img & " result" & Result'img);
--    return Result;
-- end Convert;

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
   procedure Get_Absolute_Iterate (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type) is
   ---------------------------------------------------------------

      Last_Pan                   : Absolute_Type := Absolute_Type'last;
      Last_Tilt                  : Absolute_Type := Absolute_Type'last;

   begin
      Log_In (Debug);
      loop
         declare
            Accumulator          : Interfaces.Unsigned_16;
            Conversion           : Absolute_Type;
            for Conversion'address use Accumulator'address;
            Response             : Response_Buffer_Type;
            Timeout              : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + 60.0;
         begin
            case Camera_Queue.Synchronous (
                  Command           => Position_Request,
                  Options           => Null_Options) is

               when Fault =>
                  Log_Here ("Synchronous return fault");
                  raise Failed with "Synchronous return fault";

               when Not_Set =>
                  Log_Here ("Synchronous return not set");
                  raise Failed with "Synchronous return not set";

               when Success =>
                  Log_Here (Debug, "Synchronous return Success");

               when Standard.Camera.Timeout =>
                  Log_Here ("Synchronous return Timeout");
                  raise Failed with "Synchronous return timed out";

            end case;

            if Debug then
               Response.Dump ("get absolute");
            end if;

            Accumulator := 0;
            for I in Index_Type'(3) .. 6 loop
               Log_Here (Debug, I'img & ": " &
                  Ada_lib.Socket_IO.Hex (Response.Buffer (I)));
               Accumulator := Accumulator * 16#10# +
                  Interfaces.Unsigned_16 (Response.Buffer (I) and 16#F#);
            end loop;
            Log_Here (Debug, Hex_IO.Hex (Accumulator) &
               " conversion" & Conversion'img);
            Pan := Conversion;

            Accumulator := 0;
            for I in Index_Type'(7) .. 10 loop
               Log_Here (Debug, I'img & ": " &
                  Ada_lib.Socket_IO.Hex (Response.Buffer (I)));
               Accumulator := Accumulator * 16#10# +
                  Interfaces.Unsigned_16 (Response.Buffer (I) and 16#F#);
            end loop;
            Log_Here (Debug, Hex_IO.Hex (Accumulator) &
               " conversion" & Conversion'img);
            Tilt := Conversion;

            if Last_Pan /= Absolute_Type'last then
               declare
                  Delta_Pan            : constant Integer := abs (
                                          Integer (Last_Pan) - Integer (Pan));
                  Delta_Tilt            : constant Integer := abs (
                                          Integer (Last_Tilt) - Integer (Tilt));
                  Delta_Message        : constant String :=
                                          " delta pan" & Delta_Pan'img &
                                          " delta tilt" & Delta_Tilt'img;

               begin
                  Log_Here (Debug, "Pan" & Pan'img & " Tilt" & Tilt'img &
                     Delta_Message);
                  if    Delta_Pan < 2 and then
                        Delta_Tilt < 2 then
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

            Last_Pan := Pan;
            Last_Tilt := Tilt;
         end;
      end loop;
   end Get_Absolute_Iterate;

   ---------------------------------------------------------------
   procedure Get_Response (
      Camera_Queue               : in out Camera_Queue_Type;
      Expect_Ack                 : in     Boolean;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration) is
   ---------------------------------------------------------------

      Ack_Length                 : constant Index_Type :=
                                    Camera_Queue_Type'class (
                                       Camera_Queue).Get_Ack_Length;
      Got_Ack                    : Boolean := False;
      Length                     : Index_Type := Response_Length;
      Timeout                    : constant Ada_Lib.Time.Time_Type :=
                                    Ada_Lib.Time.Now + Response_Timeout;
   begin
      Log_In (Debug, "Expect_Ack " & Expect_Ack'img &
         " ack length" & Ack_Length'img &
         " Expect_Response " & Expect_Response'img &
         " Response_Length" & Response_Length'img &
         " Response_Timeout " & Response_Timeout'img &
         " timeout " & Ada_Lib.Time.Image (Timeout));
      Camera_Queue.Read (Response (Response'first .. Ack_Length), Response_Timeout);
      if Debug then
         Video.Lib.Dump ("response", Response (Response'first .. Ack_Length),
            Natural (Ack_Length));
      end if;

      if Response (Ack_Length) = 16#FF# then -- end of Ack
         Log_Here (Debug, "got ack");
         Got_Ack := True;
      elsif Expect_Ack then
         Log_Exception (Debug, "missing Ack");
         raise Failed with "missing Ack";
      else                       -- did not get an ack
         Length := Length - Ack_Length;
      end if;

      if Expect_Response then
         declare
            End_Read          : constant Index_Type := Response_Length +
                                 (if Got_Ack then
                                    Ack_Length
                                 else
                                    0);
         begin
            Log_Here (Debug, " End_Read" & End_Read'img);

            Camera_Queue.Read (Response (Ack_Length + 1 .. End_Read),
               Response_Timeout);
            if Debug then
               Video.Lib.Dump ("response", Response ((if Got_Ack then
                     Ack_Length + 1
                  else
                     Response'first)
                   .. End_Read),
                  Natural (Response_Length));
            end if;

            if Response (End_Read) /= 16#FF# then
               Log_Exception (Debug, "missing FF at end of response");
               raise FAiled with "missing FF at end of response";
            end if;

            if Got_Ack then     -- move response up to head
               declare
                  Put         : Index_Type := Response'first;
               begin
                  for Index in Ack_Length + 1 .. End_Read loop
                     Response (Put) := Response (Index);
                     Put := Put + 1;
                  end loop;
               end;
            end if;
            if Debug then
               Video.Lib.Dump ("response", Response, Natural (Response_Length));
            end if;
         end;
      end if;
      Log_Out (Debug);
   end Get_Response;

-- ---------------------------------------------------------------
-- procedure Get_Zoom (
--    Camera_Queue               : in out Camera_Queue_Type;
--    Zoom                       :    out Absolute_Type) is
-- ---------------------------------------------------------------
--
--    Last_Zoom                   : Absolute_Type := Absolute_Type'last;
--
-- begin
--    Log_In (Debug);
--    loop
--       declare
--          Accumulator          : Interfaces.Unsigned_16;
--          Conversion           : Absolute_Type;
--          for Conversion'address use Accumulator'address;
--          Response             : Response_Buffer_Type;
--          Timeout              : constant Ada_Lib.Time.Time_Type :=
--                                  Ada_Lib.Time.Now + 60.0;
--       begin
--          case Camera_Queue.Synchronous (
--             Command           => Zoom_Inquire,
--             Options           => Null_Options) is
--
--             when Fault =>
--                Log_Here ("Synchronous return fault");
--
--             when Success =>
--                Log_Here (Debug, "Synchronous return Success");
--
--             when Standard.Camera_Queue.Timeout =>
--                Log_Here ("Synchronous return Timeout");
--
--          end case;
--
--          if Debug then
--             Response.Dump ("zoom");
--          end if;
--
--          Accumulator := 0;
--          for I in Index_Type'(3) .. 6 loop
--             Log_Here (Debug, I'img & ": " &
--                Ada_lib.Socket_IO.Hex (Response.Buffer (I)));
--             Accumulator := Accumulator * 16#10# +
--                Interfaces.Unsigned_16 (Response.Buffer (I) and 16#F#);
--          end loop;
--          Log_Here (Debug, Hex_IO.Hex (Accumulator) &
--             " conversion" & Conversion'img);
--          Zoom := Conversion;
--
--          if Last_Zoom /= Absolute_Type'last then
--             declare
--                Delta_Zoom            : constant Integer := abs (
--                                        Integer (Last_Zoom) - Integer (Zoom));
--                Delta_Message        : constant String :=
--                                        " delta zoom" & Delta_Zoom'img;
--             begin
--                Log_Here (Debug, "Zoom" & Zoom'img & Delta_Message);
--                if    Delta_Zoom < 2 then
--                   Log_Out (Debug);
--                   return;
--                elsif Ada_Lib.Time.Now > Timeout then
--                   declare
--                      Message        : constant String :=
--                                        "Get_Absolute did not converge. " &
--                                        Delta_Message;
--                   begin
--                      Log_Exception (Debug, Message);
--                      raise Failed with Message;
--                   end;
--                end if;
--             end;
--          end if;
--
--          Last_Zoom := Zoom;
--       end;
--    end loop;
-- end Get_Zoom;

-- ---------------------------------------------------------------
-- procedure Position_Relative (
--    Camera_Queue               : in out Camera_Queue_Type;
--    Pan                        : in      Relative_Type;
--    Tilt                       : in      Relative_Type;
--    Pan_Speed                  : in      Property_Type := 1;
--    Tilt_Speed                 : in      Property_Type := 1) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
--    case Camera_Queue.Synchronous (Position_Relative,
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
--    ) is
--
--       when Fault =>
--          Log_Here ("Synchronous return fault");
--
--       when Success =>
--          Log_Here (Debug, "Synchronous return Success");
--
--       when Standard.Camera_Queue.Timeout =>
--          Log_Here ("Synchronous return Timeout");
--
--    end case;
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
   procedure Process_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type) is
   ---------------------------------------------------------------

      Get_Ack                    : Boolean;
      Has_Response               : Boolean;
--    Response                   : Maximum_Response_Type;
      Response_Length            : Index_Type;

   begin
      Log_In (Debug, "command " & Command'img);
      Camera_Queue_Type'class (Camera_Queue).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      if Get_Ack or else Has_Response then
         raise Failed with "unexpected Ack " & Get_Ack'img &
            " or Has_Response " & Has_Response'img;
      end if;
      Log_Out (Debug);
   end Process_Command;

   ---------------------------------------------------------------
   overriding
   procedure Process_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type) is
   ---------------------------------------------------------------

      Get_Ack                    : Boolean;
      Has_Response               : Boolean;
      Timeout                    : constant Duration :=
                                    Camera_Queue_Type'class (Camera_Queue).
                                       Get_Timeout (Command);
   begin
      Log_In (Debug, "command " & Command'img &
         " testing " & Ada_Lib.Unit_Testing'img &
         " queue failed " & Standard.Camera.Command_Queue.Has_Queue_Failed'img &
         " timeout " & Timeout'img);
      if    Ada_Lib.Unit_Testing and then
            Standard.Camera.Command_Queue.Has_Queue_Failed then
         raise Failed with "command queue failed";
      end if;

      if Camera_Queue.Waiting_For_Response then
         raise Failed with "outstanding request " & Camera_Queue.Last_Command'img;
      end if;

      Camera_Queue_Type'class (Camera_Queue).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      Log_Here (Debug, "get ack " & Get_Ack'img &
         " return package " & Has_Response'img &
         " response length" & Response_Length'img);

      if Get_Ack or else Has_Response then
         Camera_Queue.Waiting_For_Response := True;
         Camera_Queue.Last_Command := Command;
      end if;

      Camera_Queue.Get_Response (Get_Ack, Has_Response, Response,
         Response_Length, Timeout);  -- get response

      if Debug and then Has_Response then
         Dump ("package response", Response (
            Response'first .. Response_Length));
      end if;

      Camera_Queue.Waiting_For_Response := False;
      Camera_Queue.Last_Command := No_Command;

      Log_Out (Debug);

   exception
      when Fault : others =>
         Trace_Exception (Fault, Here);
         raise;

   end Process_Command;

   ---------------------------------------------------------------
   procedure Read (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       :    out Data_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout) is
   ---------------------------------------------------------------

      Buffer                     : Buffer_Type (1 .. 1);

   begin
      Camera.Command_Queue.Queued_Camera_Type (Camera_Queue).Read (
         Buffer, Timeout);
      Data:= Buffer (Buffer'first);
   end Read;

   ---------------------------------------------------------------
   overriding
   procedure Read (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout) is
   ---------------------------------------------------------------

   begin
      Camera.Command_Queue.Queued_Camera_Type (Camera_Queue).Read (
         Data, Timeout);
   end Read;

   ---------------------------------------------------------------
   procedure Set_Absolute (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
      case Camera_Queue.Synchronous (Position_Absolute,
         Options     => (
            (
               Data           => Pan_Speed,
               Start          => 5,
               Variable_Width => False
            ),
            (
               Data           => Tilt_Speed,
               Start          => 6,
               Variable_Width => False
            ),
            (
               Start          => 7,
               Variable_Width => True,
               Value          => Convert (Pan),
               Width          => 4
            ),
            (
               Start          => 11,
               Variable_Width => True,
               Value          => Convert (Tilt),
               Width          => 4
            )
         )
      ) is

         when Fault =>
            Log_Here ("Synchronous return fault");
            raise Failed with "Synchronous command returnd fault";

         when Not_Set =>
            Log_Here ("Synchronous return not set");
            raise Failed with "Synchronous command returnd not set";

         when Success =>
            Log_Here (Debug, "Synchronous return Success");

         when Standard.Camera.Timeout =>
            Log_Here ("Synchronous return Timeout");
            raise Failed with "Synchronous command returnd timeout";

      end case;

      Log_Out (Debug);
   end Set_Absolute;

   ---------------------------------------------------------------
   procedure Set_Power (
      Camera_Queue               : in out Camera_Queue_Type;
      On                         : in     Boolean) is
   ---------------------------------------------------------------

      Data                       : constant array (Boolean) of Data_Type  := (
                                    False => 3,
                                    True  => 2);
   begin
      Log_In (Debug, "on " & On'img);
      case Camera_Queue.Synchronous (Power,
         Options     => ( 1 =>
               (
                  Data           => Data (On),
                  Start          => 5,
                  Variable_Width => False
               )
            )) is

         when Fault =>
            Log_Here ("Synchronous return fault");

         when Not_Set =>
            Log_Here ("Synchronous return not set");
            raise Failed with "Synchronous command returnd not set";

         when Success =>
            Log_Here (Debug, "Synchronous return Success");

         when Standard.Camera.Timeout =>
            Log_Here ("Synchronous return Timeout");

      end case;

      Log_Out (Debug);
   end Set_Power;

   ---------------------------------------------------------------
   procedure Set_Preset (
      Camera_Queue               : in out Camera_Queue_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img);
      case Camera_Queue.Synchronous (Memory_Recall,
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Preset_ID),
                  Start          => 6,
                  Variable_Width => False
               )
            )) is

         when Fault =>
            Log_Here ("Synchronous return fault");

         when Not_Set =>
            Log_Here ("Synchronous return not set");
            raise Failed with "Synchronous command returnd not set";

         when Success =>
            Log_Here (Debug, "Synchronous return Success");

         when Standard.Camera.Timeout =>
            Log_Here ("Synchronous return Timeout");

      end case;

      if Wait_Until_Finished then
         declare
            Pan                  : Absolute_Type;
            Tilt                 : Absolute_Type;
         begin
            Camera_Queue_Type'class (Camera_Queue).Get_Absolute (Pan, Tilt);
            Log_Here (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
         end;
      end if;
      Log_Out (Debug);
   end Set_Preset;

   ---------------------------------------------------------------
   procedure Write (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       : in     Data_Type) is
   ---------------------------------------------------------------

      Buffer                     : constant Buffer_Type (1 .. 1) := (
                                    1 => Data
                                 );
   begin
log_here;
      Camera.Command_Queue.Queued_Camera_Type (Camera_Queue).Write (Buffer);
log_here;
   end Write;

   ---------------------------------------------------------------
   overriding
   procedure Write (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       : in     Buffer_Type) is
   ---------------------------------------------------------------

   begin
log_here;
      Camera.Command_Queue.Queued_Camera_Type (Camera_Queue).Write (Data);
log_here;
   end Write;

begin
--Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Camera.Commands;
