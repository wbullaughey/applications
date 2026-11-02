with Ada.Unchecked_Deallocation;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Video.Lib;

package body Generics is

   Debug                         : Boolean renames Video.Lib.Video_Options.Generic_Debug;

   ----------------------------------------------------------------
   function Get_Default_Value (
      Video_ID                   : in     Video.Video_ID_Type
   ) return Result_Type is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "camera" & Video_ID'img);
not_implemented;
return 0;
--    declare
--       State_Pointer  : constant Configuration.Configuration_Constant_Class_Access :=
--                            Configuration.Get_Constant_Configuration_State (
--                               Video_ID);
--       State          : Configuration.Configuration_Type'class renames
--                         State_Pointer.all;
--       Result         : constant Result_Type := Accessor (State);
--
--    begin
--       Log_Out (Debug, Result'img);
--       return Result;
--    end;
   end Get_Default_Value;

   ----------------------------------------------------------------
   package body Vector_Package is

      Vector                     : Vectors_Type := Null;

      procedure Free is new Ada.Unchecked_Deallocation (
         Vector_Type, Vectors_Type);

      -------------------------------------------------------------
      procedure Allocate_Object (
         ID                      : in     ID_Type;
         Object                  : Object_Class_Access) is
      -------------------------------------------------------------

      begin
         Log_Here (Debug, "ID" & ID'img);
         Vector (ID) := Object;
      end Allocate_Object;

      -------------------------------------------------------------
      procedure Allocate_Vector (
         Max_ID                  : in     ID_Type) is
      -------------------------------------------------------------

      begin
         Log_Here (Debug, "Max ID" & Max_ID'img);
         Vector := new Vector_Type (1 .. Max_ID);
      end Allocate_Vector;

      -------------------------------------------------------------
      procedure Free_Vector is
      -------------------------------------------------------------

      begin
         Log_Here (Debug);
         Free (Vector);
      end Free_Vector;

      -------------------------------------------------------------
      function Get (
         ID                      : in     ID_Type
      ) return Object_Class_Access is
      -------------------------------------------------------------

      begin
         Log_Here (Debug, "ID" & ID'img);
         return Vector (ID);
      end Get;

      -------------------------------------------------------------
      function Get_Constant (
         ID                      : in     ID_Type
      ) return Object_Constant_Class_Access is
      -------------------------------------------------------------

      begin
         Log_Here (Debug, "ID" & ID'img);
         return Object_Constant_Class_Access (Vector (ID));
      end Get_Constant;

      -------------------------------------------------------------
      function Has_Object (
         ID                      : in     ID_Type
      ) return Boolean is
      -------------------------------------------------------------

      begin
         return Log_Here (Vector (ID) /= Null, Debug, "ID" & ID'img);
      end Has_Object;

      -------------------------------------------------------------
      function Has_Vector
      return Boolean is
      -------------------------------------------------------------

      begin
         return Log_Here (Vector /= Null, Debug);
      end Has_Vector;

      -------------------------------------------------------------
      function Iterate (
         Callback                : in     Callback_Access;
         Match                   : in     Match_Type
      ) return Object_Class_Access is
      -------------------------------------------------------------

      begin
         for Object of Vector.all loop
            if Callback (Object_Constant_Class_Access (Object), Match) then
               return Object;
            end if;
         end loop;
         return Null;
      end Iterate;

      -------------------------------------------------------------
      procedure Set_Object (
         ID                      : in     ID_Type;
         Object                  : in     Object_Class_Access) is
      -------------------------------------------------------------

      begin
         Vector (ID) := Object;
      end Set_Object;

   end Vector_Package;
end Generics;
