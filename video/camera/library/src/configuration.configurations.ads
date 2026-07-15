with Configuration.Camera.State;
with Configuration.Camera.Setup;

package Configuration.Configurations is

   type CConfigurations_Type tagged private;

private
   type CConfigurations_Type
                     is tagged record
      Camera_State   : Configuration.Camera.State.State_Class_Access;
      Camera_Setup   : Configuration.Camera.Setup.Configuration_Class_Access;
   end record;

   type Initializer_Type         is null record;

   function Allocator (
      Initializer          : in   Initializer_Type
   ) return Camera_State_Class_Access;

   package Cameras_Package       is new Ada_Lib.Maps.Table (
      Allocator         => Allocator,
      Element_Type      => Camera_State_Type,
      Element_Access    => Camera_State_Class_Access,
      Initializer_Type  => Initializer_Type,
      Name              => "camera configuration");

   subtype Camera_Set_Type       is Cameras_Package.Map;

end Configuration.Configurations;
