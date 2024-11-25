with Ada.Containers.Doubly_Linked_Lists;
--with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Event;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
--with Camera.Lib.Base;

package body Camera.Command_Queue is

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

   Completion_Description        : aliased constant String := "completion event";
   Queue_Failed                  : Boolean := False;
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
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "address " & Camera_Address.Image &
         " port" & Port_Number'img);
      Queued_Camera.Camera_Address :=
         new Ada_Lib.Socket_IO.Address_Type'(Camera_Address);
      Queued_Camera.Camera_Address.all := Camera_Address;
      Queued_Camera.Port_Number := Port_Number;
      Queued_Camera.Base_Camera.Open (Camera_Address, Port_Number);
      Log_Out (Debug);
   end Open;

-- ---------------------------------------------------------------
-- procedure Read (
--    Camera_Queue               :    out Queued_Camera_Type;
--    Data                       :    out Data_Type;
--    Timeout                    : in     Duration := Video.Lib.No_Timeout) is
-- ---------------------------------------------------------------
--
-- begin
--    Camera_Queue.Base_Camera.Read (Data, Timeout);
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

--   ---------------------------------------------------------------
--   procedure Write (
--      Camera_Queue               :    out Queued_Camera_Type;
--      Data                       : in     Data_Type) is
--   ---------------------------------------------------------------
--
--   begin
--log_here;
--      Camera_Queue.Base_Camera.Write (Data);
--log_here;
--   end Write;

   ---------------------------------------------------------------
   procedure Write (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       : in     Buffer_Type) is
   ---------------------------------------------------------------

   begin
log_here;
      Camera_Queue.Base_Camera.Write (Data);
log_here;
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

                  if Parameter.Dynamic then
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
