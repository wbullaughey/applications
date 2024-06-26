with Ada_Lib.Directory;
with ADA_LIB.Trace; use ADA_LIB.Trace;
--with Camera.Lib;

package body Widgets is

   ----------------------------------------------------------------
   function Check_Image (
      Column               : in     Configuration.Camera.Column_Type;
      Row                  : in     Configuration.Camera.Row_Type
   ) return String is
   ----------------------------------------------------------------

      Image_Name                 : constant String :=
                                    Configuration.Camera.State.
                                       Global_Camera_State.Image_Path (Row, Column);
      Image_Path        : constant String := "img/" & Image_Name;
                           -- gnoga ads img/
   begin
      Log_Here (Debug, "row" & Row'img &
         " column" & Column'img &
         Quote (" image Name", Image_Name) &
         Quote (" image path", Image_Path));

      if Image_Name'length > 0 then
         if Ada_Lib.Directory.Exists (Image_Path) then
            return Image_Path;
         else
            raise Failed with Quote ("image path", Image_Path) & " does not exist";
         end if;
      else
         return "";
      end if;
   end Check_Image;

begin
--Debug := True;
   Log_Here (Debug or Elaborate);
end Widgets;
