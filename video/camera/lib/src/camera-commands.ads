--with AUnit.Test_Suites;
with Configuration.Camera;
with Camera.Command_Queue;
--with Interfaces;
with Video.Lib;

package Camera.Commands is

   Failed                        : exception;

   type Camera_Queue_Type        is abstract new Command_Queue.
                                    Queued_Camera_Type with private;

   type Camera_Queue_Class_Access
                                 is access all Camera_Queue_Type'class;

   procedure Acked (
      Camera_Queue               : in     Camera_Queue_Type;
      Response                   : in     Response_Type;
      Value                      :    out Natural;
      Next_Buffer_Index          :    out Index_Type) is abstract;

   procedure Completed (
      Camera_Queue               : in     Camera_Queue_Type;
      Buffer                     : in     Response_Type;
      Start                      : in     Index_Type;
      Completion_Value           :    out Natural;
      Next_Byte                  :    out Index_Type) is abstract;

   procedure Get_Absolute (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type) is abstract;

   procedure Get_Absolute_Iterate (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type);

   function Get_Ack_Length (
      Camera_Queue               : in     Camera_Queue_Type
   ) return Index_Type is abstract;

   function Get_Default_Preset (
      Camera_Queue               : in     Camera_Queue_Type
   ) return Configuration.Camera.Preset_ID_Type is abstract;

   procedure Get_Power (
      Camera                     : in out Camera_Queue_Type;
      Power                      :    out Boolean) is abstract;

   function Get_Timeout (
      Camera_Queue               : in     Camera_Queue_Type;
      Command                    : in     Commands_Type
   ) return Duration is abstract;

   procedure Get_Zoom (
      Camera_Queue               : in out Camera_Queue_Type;
      Zoom                       :    out Absolute_Type) is abstract;

   -- command with no response
   overriding
   procedure Process_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type);

   -- command with response
   overriding
   procedure Process_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Response                   :    out Maximum_Response_Type;
      Response_Length            :    out Index_Type);

   procedure Position_Relative (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1) is abstract;

   procedure Process_Response (
      Camera_Queue               : in     Camera_Queue_Type;
      Response                   : in     Response_Type;
      Value                      :    out Data_Type;
      Next_Buffer_Start          :    out Index_Type) is abstract;

   procedure Read (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       :    out Data_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout);

   overriding
   procedure Read (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       :    out Buffer_Type;
      Timeout                    : in     Duration := Video.Lib.No_Timeout);

   procedure Send_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Send_Command (
      Camera_Queue               : in out Camera_Queue_Type;
      Command                    : in     Commands_Type;
      Options                    : in     Options_Type;
      Get_Ack                    :    out Boolean;
      Has_Response               :    out Boolean;
      Response_Length            :    out Index_Type) is abstract;

   procedure Set_Absolute (
      Camera_Queue               : in out Camera_Queue_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Power (
      Camera_Queue               : in out Camera_Queue_Type;
      On                         : in     Boolean);

   -- sets camera to a preset
   procedure Set_Preset (
      Camera_Queue               : in out Camera_Queue_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

   procedure Write (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       : in     Data_Type);

   overriding
   procedure Write (
      Camera_Queue               : in out Camera_Queue_Type;
      Data                       : in     Buffer_Type);

   Debug                         : Boolean := False;

private

   procedure Get_Response (
      Camera_Queue               : in out Camera_Queue_Type;
      Expect_Ack                 : in     Boolean;
      Expect_Response            : in     Boolean;
      Response                   :    out Response_Type;
      Response_Length            : in     Index_Type;
      Response_Timeout           : in     Duration);

   type Camera_Queue_Type        is abstract new Command_Queue.
                                    Queued_Camera_Type with record
      Last_Command               : Commands_Type := No_Command;
      Waiting_For_Response : Boolean := False;
   end record;

end Camera.Commands;
