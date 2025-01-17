with Ada_Lib.Socket_IO;
--with Ada_Lib.Strings;
--with ADA_LIB.Trace; use ADA_LIB.Trace;
with Camera.Lib.Base;
--with Camera.Command_Queue; use Camera.Command_Queue;
with Configuration.Camera;
with Video.Lib;

package Camera.Command_Queue is

   Failed                        : exception;

   function Has_Queue_Failed
   return Boolean
   with Pre => Is_Queue_Running;

   function Is_Queue_Running
   return Boolean;

   subtype Ack_Response_Type     is Standard.Camera.Lib.Ack_Response_Type;

   type Callback_Parameter_Type  is record
      Command_Code               : Commands_Type;
      Response_Buffer            : Video.Lib.Response_Buffer_Class_Access;
   end record;

   type Callback_Parameter_Class_Access
                                 is access all Callback_Parameter_Type;

   type Queued_Camera_Type       is abstract tagged limited private;

   type Queued_Camera_Class_Access
                                 is access all Queued_Camera_Type'class;
-- procedure Acked (
--    Camera_Queue               : in     Queued_Camera_Type;
--    Response                   : in     Response_Type;
--    Value                      :    out Natural;
--    Next_Buffer_Index          :    out Index_Type) is abstract;

   procedure Asynchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Callback_Parameter         : in     Callback_Parameter_Class_Access;
      Dynamic                    : in     Boolean  -- when true it will be freed
   ) with Pre => Is_Queue_Running and then
                 not Has_Queue_Failed;

-- -- sets camera location to a preset and waits for camera to stableize
-- procedure Checked_Move_To_Preset (
--    Camera_Queue               : in out Queued_Camera_Type;
--    Preset_ID                  : in     Configuration.Camera.Preset_ID_Type);
--
   procedure Close (
      Queued_Camera              : in out Queued_Camera_Type);

   procedure Completed (
      Camera_Queue               : in     Queued_Camera_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type) is abstract;

   procedure Get_Absolute (
      Camera_Queue               : in out Queued_Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      In_Queue                   : in     Boolean := False) is abstract;

   procedure Get_Absolute_Iterate ( -- use after move to make sure camera is stable
      Camera_Queue               : in out Queued_Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type;
      In_Queue                   : in     Boolean := False);

   function Get_Ack_Length (
      Camera_Queue               : in     Queued_Camera_Type
   ) return Index_Type is abstract;

   function Get_Default_Preset (
      Camera_Queue               : in     Queued_Camera_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   procedure Get_Power (
      Camera                     : in out Queued_Camera_Type;
      Power                      :    out Boolean) is abstract;

   function Get_Timeout (
      Camera_Queue               : in     Queued_Camera_Type;
      Command                    : in     Commands_Type
   ) return Duration is abstract;

   procedure Get_Zoom (
      Camera_Queue               : in out Queued_Camera_Type;
      Zoom                       :    out Absolute_Type) is abstract;

   function Last_Preset (
      Camera_Queue               : in     Queued_Camera_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   function Minimum_Test_Preset (
      Camera_Queue               : in     Queued_Camera_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   -- sets camera location to a preset
   procedure Move_To_Preset (
      Camera_Queue               : in out Queued_Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type) is abstract;

   procedure Open (
      Queued_Camera              : in out Queued_Camera_Type;
--    Base_Camera                : in     Lib.Base.Base_Camera_Class_Access;
      Camera_Address             : in     Ada_Lib.Socket_IO.Address_Type;
      Port_Number                : in     Ada_Lib.Socket_IO.Port_Type;
      Connection_Timeout         : in     Ada_Lib.Socket_IO.Timeout_Type := 1.0);

   procedure Position_Relative (
      Camera_Queue               : in out Queued_Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is abstract;

   procedure Process_Response (
      Camera_Queue               : in     Queued_Camera_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Index_Type) is abstract;

   procedure Reopen (
      Queued_Camera              : in out Queued_Camera_Type);

   procedure Send_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Standard.Camera.Lib.Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Send_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Standard.Camera.Lib.Ack_Response_Type;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Set_Absolute (
      Camera                     : in out Queued_Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is abstract;

   procedure Set_Power (
      Camera                     : in out Queued_Camera_Type;
      On                         : in     Boolean) is abstract;

   procedure Stop_Task;

   -- command that does not get data back from camera
   procedure Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type
   ) with Pre => Is_Queue_Running and then
               not Has_Queue_Failed;

   -- command that gets data back from camera
   procedure Synchronous (
      Queued_Camera              : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response_Buffer            : in     Response_Buffer_Class_Access
   ) with Pre => Is_Queue_Running and then
                 not Has_Queue_Failed;

   -- updates preset to current location
   procedure Update_Preset (
      Camera_Queue               : in out Queued_Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type
   ) is abstract;

   Debug                         : Boolean := False;

private

   type Queued_Camera_Type is abstract tagged limited record
      Base_Camera          : aliased Lib.Base.Base_Camera_Type;
      Camera_Address       : Ada_Lib.Socket_IO.Address_Access := Null;
      Last_Command         : Commands_Type := No_Command;
      Port_Number          : Ada_Lib.Socket_IO.Port_Type;
      Waiting_For_Response : Boolean := False;
   end record;

   procedure Get_Response (
      Camera_Queue               : in out Queued_Camera_Type;
      Expect_Ack                 : in     Standard.Camera.Lib.Ack_Response_Type;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration);

   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type);

   procedure Process_Command (
      Camera_Queue               : in out Queued_Camera_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type);

   procedure Read (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Wait_For_Move (   -- wait until camera stabalizes in one spot
                               -- only called in queue task
      Queued_Camera           : in out Queued_Camera_Type);

   procedure Write (
      Camera_Queue               : in out Queued_Camera_Type;
      Data                       : in     Buffer_Type);

end Camera.Command_Queue;
