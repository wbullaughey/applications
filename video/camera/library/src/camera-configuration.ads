--with Ada_Lib.Strings.Unlimited;use Ada_Lib.Strings.Unlimited;
with Camera.Base;
--with Camera.Main;
--limited with Camera.Configurations;
--limited with Camera.Lib.Base;
limited with Camera.Main;
--limited with Configuration.Camera.State;
--limited with Configuration.Camera.Setup;

package Camera.Configuration is

   type Configuration_Type       is new Base.Configuration_Type with private;
   type Configuration_Access     is access all Configuration_Type;
   type Configuration_Class_Access
                                 is access all Configuration_Type'class;
   type Configuration_Constant_Access
                                 is access all Configuration_Type;
   type Configuration_Constant_Class_Access
                                 is access constant Configuration_Type'class;

   function Get_Window_Connection (
      Configuration        : in     Configuration_Type
   ) return access Main.Window_Connection_Type'class
   with Pre => Configuration.Has_Window_Connection;

-- function Has_Current_Camera_ID
-- return Boolean;

   function Has_Window_Connection (
      Configuration        : in     Configuration_Type
   ) return Boolean;
--
private

   type Configuration_Type is new Base.Configuration_Type with record
      Window_Connection    : access Main.Window_Connection_Type'class := Null;
   end record;

end Camera.Configuration;
