--with Ada_Lib.Strings.Unlimited;
with Camera.Base;
with Camera.Main;
--limited with Camera.States;
--limited with Camera.Lib.Base;
--limited with Camera.Main;
with Configuration.Camera.State;
with Configuration.Camera.Setup;

package Camera.State is

   type State_Type                  is tagged private;
   type State_Access                is access State_Type;
   type State_Constant_Access       is access all State_Type;
   type State_Class_Access          is access State_Type'class;
   type State_Constant_Class_Access is access constant State_Type'class;

   procedure Load (
      Location    : in     Video.Lib.Location_Type);

   Debug                   : Boolean := False;

private

   type State_Type         is tagged record
      Camera_State         : Base.Camera_State_Access := Null;
      Configuration_Setup  : Configuration.Camera.Setup.Setup_Type;
      Configuration_State  : Configuration.Camera.State.State_Type;
      Window_Connection    : access Main.Window_Connection_Type'class := Null;
   end record;

end Camera.State;
