with Ada.Streams;
with Ada_Lib.Time;
with ADA_LIB.Trace; use ADA_LIB.Trace;
--with Camera.Lib;
with Hex_IO;
with Interfaces;

package body Camera.Commands is

   use type Ada.Streams.Stream_Element;
   use type Ada_Lib.Time.Time_Type;
   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;
-- use type Value_Type;

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
      Camera                     : in out Camera_Queue_Type;
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
            case Camera.Synchronous (
                  Command           => Position_Request,
                  Options           => Null_Options) is

               when Fault =>
                  Log_Here ("Synchronous return fault");

               when Success =>
                  Log_Here (Debug, "Synchronous return Success");

               when Standard.Camera.Timeout =>
                  Log_Here ("Synchronous return Timeout");

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
   end Get_Absolute;

-- ---------------------------------------------------------------
-- procedure Get_Zoom (
--    Camera                     : in out Camera_Queue_Type;
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
--          case Camera.Synchronous (
--             Command           => Zoom_Inquire,
--             Options           => Null_Options) is
--
--             when Fault =>
--                Log_Here ("Synchronous return fault");
--
--             when Success =>
--                Log_Here (Debug, "Synchronous return Success");
--
--             when Standard.Camera.Timeout =>
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
--    Camera                     : in out Camera_Queue_Type;
--    Pan                        : in      Relative_Type;
--    Tilt                       : in      Relative_Type;
--    Pan_Speed                  : in      Property_Type := 1;
--    Tilt_Speed                 : in      Property_Type := 1) is
-- ---------------------------------------------------------------
--
-- begin
--    Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
--    case Camera.Synchronous (Position_Relative,
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
--       when Standard.Camera.Timeout =>
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
   procedure Set_Absolute (
      Camera                     : in out Camera_Queue_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "pan" & Pan'img & " tilt" & Tilt'img);
      case Camera.Synchronous (Position_Absolute,
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
      Camera                     : in out Camera_Queue_Type;
      On                         : in     Boolean) is
   ---------------------------------------------------------------

      Data                       : constant array (Boolean) of Data_Type  := (
                                    False => 3,
                                    True  => 2);
   begin
      Log_In (Debug, "on " & On'img);
      case Camera.Synchronous (Power,
         Options     => ( 1 =>
               (
                  Data           => Data (On),
                  Start          => 5,
                  Variable_Width => False
               )
            )) is

         when Fault =>
            Log_Here ("Synchronous return fault");

         when Success =>
            Log_Here (Debug, "Synchronous return Success");

         when Standard.Camera.Timeout =>
            Log_Here ("Synchronous return Timeout");

      end case;

      Log_Out (Debug);
   end Set_Power;

   ---------------------------------------------------------------
   procedure Set_Preset (
      Camera                     : in out Camera_Queue_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True) is
   ---------------------------------------------------------------

   begin
      Log_In (Debug, "preset id" & Preset_ID'img);
      case Camera.Synchronous (Memory_Recall,
         Options     => ( 1 =>
               (
                  Data           => Data_Type (Preset_ID),
                  Start          => 6,
                  Variable_Width => False
               )
            )) is

         when Fault =>
            Log_Here ("Synchronous return fault");

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
            Camera.Get_Absolute (Pan, Tilt);
            Log_Here (Debug, "pan " & Pan'img & " tilt " & Tilt'img);
         end;
      end if;
      Log_Out (Debug);
   end Set_Preset;

begin
--Debug := True;
   Log_Here (Debug or Trace_Options or Elaborate);
end Camera.Commands;
