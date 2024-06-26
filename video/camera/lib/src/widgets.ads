with Configuration.Camera.State;

package Widgets is

   Failed                        : exception;

   subtype Column_Index_Type     is Configuration.Camera.Column_Type;
   subtype Row_Index_Type        is Configuration.Camera.Row_Type;

   use type Column_Index_Type;
   use type Row_Index_Type;

   function Check_Image (
      Column               : in     Column_Index_Type;
      Row                  : in     Row_Index_Type
   ) return String
   with Pre =>
      Configuration.Camera.State.Global_State_Is_Set and then
      Row <= Configuration.Camera.State.Global_Camera_State.Images.all'last (1) and then
      Column <= Configuration.Camera.State.Global_Camera_State.Images.all'last (2);

   Debug                         : Boolean := False;

end Widgets;
