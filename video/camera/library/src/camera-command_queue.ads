with Camera.Commands; use Camera.Commands;

package Camera.Command_Queue is

   Failed                        : exception;

   type Parameters_Type (
      Command                    : Commands_Type) is record

      case Command is

         when Position_Relative =>
            Pan                  : Relative_Type;
            Tilt                 : Relative_Type;
            Pan_Speed            : Property_Type;
            Tilt_Speed           : Property_Type;

         when others =>
            null;

      end case;
   end record;

   task Process_Queue_Task is

      entry Command (
         Camera                  : in out Standard.Camera.Commands.Camera_Type'class;
         Parameters              : in     Parameters_Type);

      entry Stop;

   end Process_Queue_Task;

-- procedure Relative_Command (
--    Camera                     : in out Standard.Camera.Commands.Camera_Type'class;
--    Pan                        : in     Relative_Type;
--    Tilt                       : in     Relative_Type;
--    Pan_Speed                  : in     Property_Type := 1;
--    Tilt_Speed                 : in     Property_Type := 1);

   procedure Stop_Task;

   Debug                         : Boolean := False;

end Camera.Command_Queue;
