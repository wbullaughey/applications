with Ada.Containers.Doubly_Linked_Lists;
with Ada.Streams;
--with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Event;
with Ada_Lib.Time;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
--with Camera.Lib.Base;
with Hex_IO;
with Interfaces;

package body Camera.Command_Queue is

   use type Ada_Lib.Time.Time_Type;
   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;
   use type Standard.Camera.Lib.Ack_Response_Type;
   use type Interfaces.Integer_16;
   use type Interfaces.Unsigned_16;

   subtype Ack_Response_Type     is Standard.Camera.Lib.Ack_Response_Type;
   type Completion_Event_Type    is new Ada_Lib.Event.Event_Type with record
      Status                     : Status_Type;
   end record;

   Event_Description             : aliased constant String := "completion event";

   type Parameters_Type (
      Dynamic                    : Boolean) is limited record
      Queued_Camera              : Queued_Camera_Class_Access := Null;
      Command_Code               : Commands_Type;
      Options                    : Maximum_Options_Type;
      Last_Option                : Index_Type;
      Callback_Parameter         : Callback_Parameter_Class_Access := Null;
      Completion_Event           : Completion_Event_Type (
         Description => Event_Description'access);
   end record;

   type Parameters_Access        is access all Parameters_Type;

   procedure Free is new Ada.Unchecked_Deallocation (
     Parameters_Type, Parameters_Access);

   procedure Initialize_Parameter (
      Parameter                  :    out Parameters_Type;
      Camera                     : in     Queued_Camera_Class_Access;
      Command_Code               : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access);

   task Process_Queue_Task is

--    entry Pop_Command;

      entry Push_Command (
         Parameter               : in     Parameters_Access);

      entry Stop;

   end Process_Queue_Task;

-- Completion_Description        : aliased constant String := "completion event";
   No_Ack                        : Standard.Camera.Lib.Ack_Response_Type renames
                                    Standard.Camera.Lib.None;
   Optional_Ack                  : Standard.Camera.Lib.Ack_Response_Type renames
                                    Standard.Camera.Lib.Optional;
   Queue_Failed                  : Boolean := False;
   Required_Ack                  : Standard.Camera.Lib.Ack_Response_Type renames
                                    Standard.Camera.Lib.Required;
   Task_Running                  : Boolean := False;

--   ----------------------------------------------------------------
--   procedure Dump (
--      Response                   : in     Response_Type;
--      From                       : in     String := Here) is
--   ----------------------------------------------------------------
--
--   begin
--not_implemented;
--   end Dump;

   ---------------------------------------------------------------
   procedure Get_Absolute_Iterate (
      Camera_Queue               : in out Queued_Camera_Type;
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
      Camera_Queue               : in out Queued_Camera_Type;
      Expect_Ack                 : in     Ack_Response_Type;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration) is
   ---------------------------------------------------------------

      Ack_Length                 : constant Index_Type :=   -- camera type length
                                    Queued_Camera_Type'class (
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
         if Expect_Ack = No_Ack then
            raise Failed with "got unexpected ack";
         else
            Log_Here (Debug, "got ack");
            Length := Length - Ack_Length;
            Got_Ack := True;
         end if;
      elsif Expect_Ack = Required_Ack then
         Log_Exception (Debug, "missing Ack");
         raise Failed with "missing Ack";
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

   exception
      when Fault : others =>
         Log_Exception (Debug, Fault);
         raise;

   end Get_Response;

   ----------------------------------------------------------------
   function Has_Queue_Failed
   return Boolean is
   ----------------------------------------------------------------

   begin
      return Queue_Failed;
   end Has_Queue_Failed;

   ----------------------------------------------------------------
   function Is_Queue_Running
   return Boolean is
   ----------------------------------------------------------------

   begin
      return Task_Running;
   end Is_Queue_Running;

   ----------------------------------------------------------------
   procedure Stop_Task is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      Process_Queue_Task.Stop;
   end Stop_Task;

   ----------------------------------------------------------------
   procedure Asynchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access;
      Dynamic                    : in     Boolean) is  -- when true it will be freeds) is
   ----------------------------------------------------------------

      Parameter                  : constant Parameters_Access :=
                                    new Parameters_Type (Dynamic  => True);

   begin
      Log_In (Debug, "command " & Command'img);
      Initialize_Parameter (
         Parameter            => Parameter.all,
         Camera               => Queued_Camera'Unchecked_Access,
         Command_Code         => Command,
         Options              => Options,
         Callback_Parameter   => Callback_Parameter);

      Process_Queue_Task.Push_Command (Parameter);
      Log_Out (Debug);
   end Asynchronous;

   ----------------------------------------------------------------
   procedure Close (
      Queued_Camera              : in out Queued_Camera_Type) is
   ----------------------------------------------------------------

   begin
      Queued_Camera.Base_Camera.Close;
   end Close;

   ----------------------------------------------------------------
   procedure Initialize_Parameter (
      Parameter                  :    out Parameters_Type;
      Camera                     : in     Queued_Camera_Class_Access;
      Command_Code               : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access) is
   ----------------------------------------------------------------


   begin
      Parameter.Callback_Parameter := Callback_Parameter;
      Parameter.Command_Code := Command_Code;
      Parameter.Completion_Event.Status := Success;
      Parameter.Options (Options'first .. Options'last) := Options;
      Parameter.Last_Option := Options'last;
      Parameter.Queued_Camera := Camera;
      Parameter.Completion_Event.Status := Video.Lib.Not_Set;
   end Initialize_Parameter;

   ----------------------------------------------------------------
   procedure Open (
      Queued_Camera              : in out Queued_Camera_Type;
--    Base_Camera                : in     Lib.Base.Base_Camera_Class_Access;
      Camera_Address             : in     Ada_Lib.Socket_IO.Address_Type;
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "address " & Camera_Address.Image &
         " port" & Port_Number'img);
      Queued_Camera.Camera_Address :=
         new Ada_Lib.Socket_IO.Address_Type'(Camera_Address);
      Queued_Camera.Camera_Address.all := Camera_Address;
      Queued_Camera.Port_Number := Port_Number;
      Queued_Camera.Base_Camera.Open (Camera_Address, Port_Number, Connection_Timeout);
      Log_Out (Debug);
   end Open;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type) is
   ---------------------------------------------------------------

      Get_Ack                    : Ack_Response_Type;
      Has_Response               : Boolean;
      Response                   : Maximum_Response_Type;
      Response_Length            : Index_Type;
      Timeout                    : constant Duration :=
                                    Queued_Camera_Type'class (Camera_Queue).
                                       Get_Timeout (Command);

   begin
      Log_In (Debug, "command " & Command'img);
      Queued_Camera_Type'class (Camera_Queue).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      if Has_Response then
         declare
            Message     : constant String := "unexpected has response " &
                           Has_Response'img & " for " & Command'img;
         begin
            Log_Exception (Debug, Message);
            raise Failed with Message;
         end;
      end if;
      if Camera_Queue.Waiting_For_Response then
         declare
            Message  : constant String :=
                        "outstanding request " & Camera_Queue.Last_Command'img;
         begin
            Log_Exception (Debug, Message);
            raise Failed with Message;
         end;
      end if;

      if Get_Ack /= No_Ack then
         Camera_Queue.Waiting_For_Response := True;   -- make sure not nested
         Camera_Queue.Last_Command := Command;
      end if;

      Camera_Queue.Get_Response (Get_Ack, Has_Response, Response,
         Response_Length, Timeout);  -- get response

      Camera_Queue.Waiting_For_Response := False;
      Camera_Queue.Last_Command := No_Command;
      Log_Out (Debug);

   exception
      when Fault: others =>
         Log_Exception (Debug, Fault);
         raise;

   end Process_Command;

   ---------------------------------------------------------------
   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type) is
   ---------------------------------------------------------------

      Get_Ack                    : Ack_Response_Type;
      Has_Response               : Boolean;
      Timeout                    : constant Duration :=
                                    Queued_Camera_Type'class (Camera_Queue).
                                       Get_Timeout (Command);
   begin
      Log_In (Debug, "command " & Command'img &
         " testing " & Ada_Lib.Unit_Testing'img &
         " queue failed " & Standard.Camera.Command_Queue.Has_Queue_Failed'img &
         " timeout " & Timeout'img);
      if    Ada_Lib.Unit_Testing and then
            Standard.Camera.Command_Queue.Has_Queue_Failed then
         Log_Exception (Debug, "command queue failed");
         raise Failed with "command queue failed";
      end if;

      if Camera_Queue.Waiting_For_Response then
         declare
            Message  : constant String :=
                        "outstanding request " & Camera_Queue.Last_Command'img;
         begin
            Log_Exception (Debug, Message);
            raise Failed with Message;
         end;
      end if;

      Queued_Camera_Type'class (Camera_Queue).Send_Command (Command, Options,
         Get_Ack, Has_Response, Response_Length);
      Log_Here (Debug, "get ack " & Get_Ack'img &
         " return package " & Has_Response'img &
         " response length" & Response_Length'img);

      if Get_Ack /= No_Ack or else Has_Response then
         Camera_Queue.Waiting_For_Response := True;   -- make sure not nested
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
         Camera_Queue.Waiting_For_Response := False;  -- not nested
         Log_Exception (Debug, Fault);
         raise;

   end Process_Command;

-- ---------------------------------------------------------------
-- procedure Read (
--    Camera_Queue               : in out Queued_Camera_Type;
--    Data                       :    out Data_Type;
--    Timeout                    : in     Duration := Video.Lib.No_Timeout) is
-- ---------------------------------------------------------------
--
--    Buffer                     : Buffer_Type (1 .. 1);
--
-- begin
--    Camera_Queue.Read (Buffer, Timeout);
--    Data:= Buffer (Buffer'first);
-- end Read;

   ---------------------------------------------------------------
   procedure Read (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout) is
   ---------------------------------------------------------------

   begin
      Camera_Queue.Base_Camera.Read (Data, Timeout);
   end Read;

   ----------------------------------------------------------------
   procedure Reopen (
      Queued_Camera              : in out Queued_Camera_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Queued_Camera.Base_Camera.Close;
      Queued_Camera.Base_Camera.Reopen (Queued_Camera.Camera_Address.all,
         Queued_Camera.Port_Number);
      Log_Out (Debug);
   end Reopen;

   ----------------------------------------------------------------
   -- command that does not get data back from camera
   function Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) return Status_Type is
   ----------------------------------------------------------------

--    Callback_Parameter         : aliased Callback_Parameter_Type := (
--       Command_Code      => Command,
--       Response_Buffer   => Null);
--    Completion_Event           : Completion_Event_Type (
--          Description => Completion_Description'access) := (
--       Ada_Lib.Event.Event_Type with
--       Description => Completion_Description'access,
--       Status      => Not_Set);
      Parameter                  : aliased Parameters_Type (
         Dynamic  => False);

   begin
      Log_In (Debug, "command " & Command'img & " length" & Options'length'img);
      Initialize_Parameter (
         Parameter            => Parameter,
         Camera               => Queued_Camera'Unchecked_Access,
         Command_Code         => Command,
         Options              => Options,
         Callback_Parameter   => null);
      Process_Queue_Task.Push_Command (Parameter'unchecked_access);
      Parameter.Completion_Event.Wait_For_Event;
      Log_Out (Debug, "status " & Parameter.Completion_Event.Status'img);
      return Parameter.Completion_Event.Status;

   exception

      when Error: others =>
         Queue_Failed := True;
         Trace_Exception (Debug, Error);
         return Fault;

   end Synchronous;

   ----------------------------------------------------------------
   -- command that gets data back from camera
   function Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response_Buffer            :    out Response_Buffer_Type
   ) return Status_Type is
   ----------------------------------------------------------------

      type Response_Type is new Response_Buffer_Type with null record;

      overriding
      function Callback (
         Response                  : in out Response_Type
      ) return Status_Type;

--    Description                : aliased constant String := "completion event";
      Parameter                  : aliased Parameters_Type (
         Dynamic  => False);

      -------------------------------------------------------------
      overriding
      function Callback (
         Response                  : in out Response_Type
      ) return Status_Type is
      -------------------------------------------------------------

      begin
         Log_In (Debug);
         Response_Buffer := Response_Buffer_Type (Response);
         Log_Out (Debug);
         return Fault;
      end Callback;
      -------------------------------------------------------------

      Callback_Parameter   : aliased Callback_Parameter_Type := (
         Command_Code      => Command,
         Response_Buffer   => Response_Buffer'unchecked_access);

      Maximum_Options   : Maximum_Options_Type := (others => Null_Option);

   begin
      Log_In (Debug, "command " & Command'img);
      Initialize_Parameter (
         Parameter            => Parameter,
         Camera               => Queued_Camera'Unchecked_Access,
         Command_Code         => Command,
         Options              => Options,
         Callback_Parameter   => Callback_Parameter'unchecked_access);
      Maximum_Options (Maximum_Options'first .. Options'length) := Options;
      Process_Queue_Task.Push_Command (Parameter'unchecked_access);
      Parameter.Completion_Event.Wait_For_Event;
      Log_Out (Debug, "status " & Parameter.Completion_Event.Status'img);
      return Parameter.Completion_Event.Status;

   exception

      when Error: others =>
         Queue_Failed := True;
         Trace_Exception (Debug, Error);
         return Fault;

   end Synchronous;

-- ---------------------------------------------------------------
-- procedure Write (
--    Camera_Queue               : in out Queued_Camera_Type;
--    Data                       : in     Data_Type) is
-- ---------------------------------------------------------------
--
--    Buffer                     : constant Buffer_Type (1 .. 1) := (
--                                  1 => Data
--                               );
-- begin
--    Camera_Queue.Write (Buffer);
-- end Write;

   ---------------------------------------------------------------
   procedure Write (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       : in     Buffer_Type) is
   ---------------------------------------------------------------

   begin
      Camera_Queue.Base_Camera.Write (Data);
   end Write;

   ----------------------------------------------------------------
   package Queue_Package is new Ada.Containers.Doubly_Linked_Lists (
      Element_Type      => Parameters_Access);
   ----------------------------------------------------------------

   task body Process_Queue_Task is

      Queue                : Queue_Package.List;

   begin
      Log_In (Debug, "started");
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);
      Task_Running := True;
      loop
         select
            accept Push_Command (
               Parameter   : Parameters_Access) do

               Log_Here (Debug, "push " & Parameter.Command_Code'img);
               Queue.Append (Parameter);
            end Push_Command;
         or
            accept Stop;
               Log_Here (Debug);
               exit;
         else
            if not Queue.Is_Empty then
               declare
                  Parameter   : Parameters_Access := Queue.First_Element;
                  Dynamic     : constant Boolean := Parameter.Dynamic;
                  Response    : Response_Buffer_Type;

               begin
                  Log_Here (Debug, "pop " & Parameter.Command_Code'img);
                  Parameter.Queued_Camera.Process_Command (
                     Parameter.Command_Code, Parameter.Options (
                        Parameter.Options'first .. Parameter.Last_Option),
                     Response.Buffer, Response.Length);
                  if Parameter.Callback_Parameter = Null then
                     Parameter.Completion_Event.Status := Success;
                  else
                     case Response.Callback is

                        when Fault =>
                           Log_Here ("callback failed");
                           Parameter.Completion_Event.Status := Fault;

                        when Not_Set =>
                           Log_Here ("parameter not set");
                           Parameter.Completion_Event.Status := Fault;

                        when Success =>
                           Log_Here (Debug, "callback succeeded");
                           Parameter.Completion_Event.Status := Success;

                        when Timeout =>
                           Log_Here ("callback timed out");
                           Parameter.Completion_Event.Status := Timeout;

                     end case;
                  end if;
                  Parameter.Completion_Event.Set_Event;
                  -- don't access parameter after event if not dynamic
                  if Dynamic then
                     Free (Parameter);
                  end if;

                  Queue.Delete_First;

               exception
                  when Error : others =>
                     Trace_Exception (Error, Here);
                     Parameter.Completion_Event.Status := Fault;
                     raise;

               end;
            end if;
         end select;
      end loop;
      Task_Running := False;
      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Debug, "task terminate");
   end Process_Queue_Task;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Camera.Command_Queue;
