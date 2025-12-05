with Ada.Exceptions;
with Ada_Lib.Strings;
with Camera.Commands;
with GNAT.Source_Info;
with Gnoga.Gui.Window;

package Camera.Base is

   Failed                        : exception;

   type Camera_State_Type is abstract tagged limited private;
   type Camera_Read_Only_State_Access is access constant Camera_State_Type;
   type Camera_State_Access is access all Camera_State_Type;
   type Camera_State_Class_Access is access all Camera_State_Type'class;
   type Camera_Ready_Only_State_Class_Access is
      access constant Camera_State_Type'class;

   function Allocate_Camera_State
   return Camera_State_Class_Access;

   function Get_Camera (
      Camera_State      : in     Camera_State_Type
   ) return Camera.Commands.Camera_Class_Access is abstract;

   function Get_Camera_Name (
      Camera_State      : in     Camera_State_Type
   ) return String is abstract;

   function Get_Camera_ID (
      Camera_State      : in     Camera_State_Type
   ) return Camera_ID_Type is abstract;

   function Get_Camera_State_Pan_Speed (
      Camera_State      : in     Camera_State_Type
   ) return Data_Type is abstract;

   function Get_Camera_State_Tilt_Speed (
      Camera_State      : in     Camera_State_Type
   ) return Data_Type is abstract;

   procedure Set_Mouse_Action (
      Camera_State      : in     Camera_State_Type;
      Action            : in     Mouse_Click_Action_Type
   ) is Abstract;

   procedure Halt;

   procedure Open_Camera (
      Camera         : in out Camera_State_Type;
      Description    : in     Ada_Lib.Strings.String_Constant_Access);

   procedure Report_Exception (
      Window               : in out Gnoga.Gui.Window.Window_Type'class;
      Fault                : in     Ada.Exceptions.Exception_Occurrence;
      Message              : in     String;
      Where                : in     String := GNAT.Source_Info.Source_Location);

   Debug                         : Boolean := False;

private

   type Camera_State_Type is abstract tagged limited null record;

end Camera.Base;
