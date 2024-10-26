with Ada.Containers.Doubly_Linked_Lists;
--with Ada.Text_IO;
with Ada_Lib.Event;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
--with Camera.Commands; --use Camera.Commands;
--with Camera.Lib.Base;

package body Camera.Command_Queue is

   type Parameters_Type          is record
      Base_Camera                : Lib.Base.Base_Camera_Class_Access;
      Command_Code               : Commands_Type;
      Options                    : Maximum_Options_Type;
      Callback                   : Callback_Parameter_Class_Access := Null;
   end record;

   type Parameters_Access        is access all Parameters_Type;

   task Process_Queue_Task is

      entry Pop_Command;

      entry Push_Command (
         Parameter               : in     Parameters_Type);

      entry Stop;

   end Process_Queue_Task;

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
      Camera                     : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access;
      Dynamic                    : in     Boolean) is  -- when true it will be freeds) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "command " & Command'img);
      Process_Queue_Task.Push_Command (
         Parameters_Type'(
            Base_Camera    => Camera.Base_Camera,
            Callback       => Callback_Parameter,
            Command_Code   => Command,
            Options        => Maximum_Options_Type'(
               others => Null_Option)));
      Log_Out (Debug);
   end Asynchronous;

   ----------------------------------------------------------------
   procedure Open (
      Queued_Camera              : in out Queued_Camera_Type;
      Base_Camera                : in     Lib.Base.Base_Camera_Class_Access;
      Camera_Address             : in     Ada_Lib.Socket_IO.Address_Type;
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug);
      Queued_Camera.Base_Camera := Base_Camera;
      Base_Camera.Open (Camera_Address, Port_Number);
      Log_Out (Debug);
   end Open;

   ----------------------------------------------------------------
   function Synchronous (
      Camera                     : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) return Status_Type is
   ----------------------------------------------------------------

      Maximum_Options   : Maximum_Options_Type := (others => Null_Option);

   begin
      Log_In (Debug, "command " & Command'img & " length" & Options'length'img);
      Maximum_Options (Maximum_Options'first .. Options'length) := Options;
      Process_Queue_Task.Push_Command (
         Parameters_Type'(
            Callback       => Null,
            Base_Camera    => Camera.Base_Camera,
            Command_Code   => Command,
            Options        => Maximum_Options));
      Log_Out (Debug);
      return Success;

   exception

      when Error: others =>
         Trace_Exception (Debug, Error);
         return Fault;

   end Synchronous;

   ----------------------------------------------------------------
   function Synchronous (
      Camera                     : in out Queued_Camera_Type;
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

      Description                : aliased constant String := "completion event";
      Completion_Event           : Ada_Lib.Event.Event_Type (
                                    Description'unchecked_access);

      -------------------------------------------------------------
      overriding
      function Callback (
         Response                  : in out Response_Type
      ) return Status_Type is
      -------------------------------------------------------------

      begin
         Log_In (Debug);
         Completion_Event.Set_Event;
         Response_Buffer := Response_Buffer_Type (Response);
         Log_Out (Debug);
         return Fault;
      end Callback;
      -------------------------------------------------------------

      Maximum_Options   : Maximum_Options_Type := (others => Null_Option);

   begin
      Log_In (Debug, "command " & Command'img);
      Maximum_Options (Maximum_Options'first .. Options'length) := Options;
      Process_Queue_Task.Push_Command (
         Parameters_Type'(
            Base_Camera    => Camera.Base_Camera,
            Callback       => Null,
            Command_Code   => Command,
            Options        => Maximum_Options));
      Completion_Event.Wait_For_Event;
      Log_Out (Debug);
      return Success;

   exception

      when Error: others =>
         Trace_Exception (Debug, Error);
         return Fault;

   end Synchronous;
   ----------------------------------------------------------------

   package Queue_Package is new Ada.Containers.Doubly_Linked_Lists (
      Element_Type      => Parameters_Type);

   ----------------------------------------------------------------
   task body Process_Queue_Task is

      Queue                : Queue_Package.List;

   begin
      Log_In (Debug, "started");
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);
      Task_Running := True;
      loop
         select
            when not Queue.Is_Empty =>
               accept Pop_Command do
                  declare
                     Parameter   : constant Parameters_Type := Queue.First_Element;
                     Response    : Response_Buffer_Type;

                  begin
                     Log_Here (Debug, "pop " & Parameter.Command_Code'img);
                     Parameter.Base_Camera.Process_Command (
                        Parameter.Command_Code, Parameter.Options,
                        Response.Buffer, Response.Length);
                     if Parameter.Callback /= Null then
                        case Response.Callback is

                           when Fault =>
                              Log_Here ("callback failed");

                           when Success =>
                              Log_Here (Debug, "callback succeeded");

                           when Timeout =>
                              Log_Here ("callback timed out");

                        end case;
                     end if;

                     Queue.Delete_First;
                  end;
               end Pop_Command;
         or
            accept Push_Command (
               Parameter   : Parameters_Type) do

               Log_Here (Debug, "push " & Parameter.Command_Code'img);
               Queue.Append (Parameter);
            end Push_Command;
         or
            accept Stop;
               Log_Here (Debug);
               exit;
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
