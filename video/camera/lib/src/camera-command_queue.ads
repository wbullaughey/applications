with Ada_Lib.Socket_IO;
with Ada_Lib.Strings;
--with ADA_LIB.Trace; use ADA_LIB.Trace;
with Camera.Lib.Base;
--with Camera.Commands; use Camera.Commands;
with Video.Lib;

package Camera.Command_Queue is

   Failed                        : exception;

   function Has_Queue_Failed
   return Boolean
   with Pre => Is_Queue_Running;

   function Is_Queue_Running
   return Boolean;

   type Callback_Parameter_Type  is record
      Command_Code               : Commands_Type;
      Response_Buffer            : Video.Lib.Response_Buffer_Access;
   end record;

   type Callback_Parameter_Class_Access
                                 is access all Callback_Parameter_Type;

   type Queued_Camera_Type       is abstract tagged limited private;

   type Queued_Camera_Class_Access
                                 is access all Queued_Camera_Type'class;
   procedure Asynchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access;
      Dynamic                    : in     Boolean  -- when true it will be freed
   ) with Pre => Is_Queue_Running and then
                 not Has_Queue_Failed;

   procedure Close (
      Queued_Camera              : in out Queued_Camera_Type);

   procedure Open (
      Queued_Camera              : in out Queued_Camera_Type;
--    Base_Camera                : in     Lib.Base.Base_Camera_Class_Access;
      Camera_Address             : in     Ada_Lib.Socket_IO.Address_Type;
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0);

   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type) is abstract;

   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type) is abstract;

-- procedure Read (
--    Camera_Queue               :    out Queued_Camera_Type;
--    Data                       :    out Data_Type;
--    Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Read (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Reopen (
      Queued_Camera              : in out Queued_Camera_Type);

   procedure Stop_Task;

   -- command that does not get data back from camera
   function Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) return Status_Type
   with Pre => Is_Queue_Running and then
               not Has_Queue_Failed;

   -- command that gets data back from camera
   function Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response_Buffer            :    out Response_Buffer_Type
   ) return Status_Type
   with Pre => Is_Queue_Running and then
               not Has_Queue_Failed;

-- procedure Write (
--    Camera_Queue               :    out Queued_Camera_Type;
--    Data                       : in     Data_Type);

   procedure Write (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       : in     Buffer_Type);

   Debug                         : Boolean := False;

private

   type Queued_Camera_Type is abstract tagged limited record
      Base_Camera          : aliased Lib.Base.Base_Camera_Type;
      Camera_Address       : Ada_Lib.Socket_IO.Address_Access := Null;
      Port_Number          : Ada_Lib.Socket_IO.Port_Type;
   end record;

end Camera.Command_Queue;
