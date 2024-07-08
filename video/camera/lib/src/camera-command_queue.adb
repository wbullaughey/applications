with Ada.Text_IO;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with Ada_Lib.Trace_Tasks;
with Camera.Lib.Base;

package body Camera.Command_Queue is

   type Parameters_Type (
      Command                    : Camera.Lib.Base.Commands_Type) is record

      case Command is

         when Camera.Lib.Base.Position_Relative =>
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

   ----------------------------------------------------------------
   procedure Stop_Task is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug);
      Process_Queue_Task.Stop;
   end Stop_Task;

   ----------------------------------------------------------------
   procedure Relative_Command (
      Camera                     : in out Standard.Camera.Commands.Camera_Type'class;
      Pan                        : in     Relative_Type;
      Tilt                       : in     Relative_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "pan " & Pan'img & " speed" & Pan_Speed'img &
         " tilt " & Tilt'img & Tilt_Speed'img);
      Process_Queue_Task.Command (Camera, Parameters_Type'(
         Command     => Standard.Camera.Lib.Base.Position_Relative,
         Pan         => Pan,
         Tilt        => Tilt,
         Pan_Speed   => Pan_Speed,
         Tilt_Speed  => Tilt_Speed));
      Log_Out (Debug);
   end Relative_Command;

   ----------------------------------------------------------------
   task body Process_Queue_Task is

   begin
      Log_In (Debug, "started");
      Ada_Lib.Trace_Tasks.Start ("timer task", Here);
      loop
         select
            accept Command (
               Camera               : in out Standard.Camera.Commands.Camera_Type'class;
               Parameters           : in     Parameters_Type) do

               Log_In (Debug, "command " & Parameters.Command'img);

               case Parameters.Command is

                  when Standard.Camera.Lib.Base.Position_Relative =>
                     Camera.Position_Relative (Parameters.Pan,
                        Parameters.Tilt, Parameters.Pan_Speed,
                        Parameters.Tilt_Speed);

                  when others =>
                     raise Failed with "command " & Parameters.Command'img &
                        " not implemented";

               end case;
               Log_Out (Debug);
            end Command;
         or
            accept Stop;
               Log_Here (Debug);
               exit;
         end select;
      end loop;
      Ada_Lib.Trace_Tasks.Stop;
      Log_Out (Debug, "task terminate");

   end Process_Queue_Task;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Camera.Command_Queue;
