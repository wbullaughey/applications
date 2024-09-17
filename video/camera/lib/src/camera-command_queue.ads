with Ada_Lib.Socket_IO;
--with ADA_LIB.Trace; use ADA_LIB.Trace;
with Camera.Lib.Base;
--with Camera.Commands; use Camera.Commands;

package Camera.Command_Queue is

   Failed                        : exception;

   function Has_Queue_Failed
   return Boolean
   with Pre => Is_Queue_Running;

   function Is_Queue_Running
   return Boolean;

   type Callback_Parameter_Type  is abstract tagged record
      Command_Code               : Commands_Type;
      Response_Buffer            : Response_Buffer_Type;
   end record;

   type Callback_Parameter_Class_Access
                                 is access all Callback_Parameter_Type;

   type Queued_Camera_Type       is tagged private;

   type Queued_Camera_Class_Access
                                 is access all Queued_Camera_Type;
   procedure Asynchronous (
      Camera                     : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access;
      Dynamic                    : in     Boolean  -- when true it will be freed
   ) with Pre => Is_Queue_Running and then
                 not Has_Queue_Failed;

   procedure Open (
      Queued_Camera              : in out Queued_Camera_Type;
      Base_Camera                : in     Lib.Base.Base_Camera_Class_Access;
      Camera_Address             : in     Ada_Lib.Socket_IO.Address_Type;
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type);

   procedure Stop_Task;

   function Synchronous (
      Camera                     : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) return Status_Type
   with Pre => Is_Queue_Running and then
               not Has_Queue_Failed;

   function Synchronous (
      Camera                     : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response_Buffer            :    out Response_Buffer_Type
   ) return Status_Type
   with Pre => Is_Queue_Running and then
               not Has_Queue_Failed;

   Debug                         : Boolean := False;

private

   type Queued_Camera_Type is tagged record
      Base_Camera          : aliased Lib.Base.Base_Camera_Class_Access := Null;
   end record;

end Camera.Command_Queue;
