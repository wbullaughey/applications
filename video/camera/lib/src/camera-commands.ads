--with AUnit.Test_Suites;
with Configuration.Camera;
with Camera.Command_Queue;
--with Interfaces;
--with Video.Lib;

package Camera.Commands is

   Failed                        : exception;

-- use type Data_Type;
-- use type Value_Type;

   type Camera_Type              is new Command_Queue.
                                    Queued_Camera_Type with null record;

   type Camera_Class_Access      is access all Camera_Type'class;

   procedure Get_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        :    out Absolute_Type;
      Tilt                       :    out Absolute_Type);

   procedure Get_Zoom (
      Camera                     : in out Camera_Type;
      Zoom                       :    out Absolute_Type);

   procedure Position_Relative (
      Camera                     : in out Camera_Type;
      Pan                        : in      Relative_Type;
      Tilt                       : in      Relative_Type;
      Pan_Speed                  : in      Property_Type := 1;
      Tilt_Speed                 : in      Property_Type := 1);

   procedure Set_Absolute (
      Camera                     : in out Camera_Type;
      Pan                        : in     Absolute_Type;
      Tilt                       : in     Absolute_Type;
      Pan_Speed                  : in     Property_Type := 1;
      Tilt_Speed                 : in     Property_Type := 1);

   procedure Set_Power (
      Camera                     : in out Camera_Type;
      On                         : in     Boolean);

   -- sets camera to a preset
   procedure Set_Preset (
      Camera                     : in out Camera_Type;
      Preset_ID                  : in     Configuration.Camera.Preset_ID_Type;
      Wait_Until_Finished        : in     Boolean := True);

   Debug                         : Boolean := False;

end Camera.Commands;
