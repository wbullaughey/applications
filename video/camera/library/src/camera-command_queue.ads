with Camera.Commands; use Camera.Commands;

package Camera.Command_Queue is

   Failed                        : exception;

   procedure Relative_Command (
      Camera                     : in out Standard.Camera.Commands.Camera_Type'class;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Stop_Task;

   Debug                         : Boolean := False;

end Camera.Command_Queue;
