--with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada_Lib.Trace; use Ada_Lib.Trace;
--with Video.Lib;

package body Video.Generic_Package is

   package body One_Dimension_Vector is

      type Vector_Access is access Generic_Package_Vector_Type;

      procedure Free is new Ada.Unchecked_Deallocation (
         Name     => Generic_Package_Class_Access,
         Object   => Generic_Package_Type'class);

      Vector           : Vector_Access := Null;

      ----------------------------------------------------------------
      procedure Allocate_Vector (
         Vector_Size   : in     Vector_Index_Type) is
      ----------------------------------------------------------------

      begin
         Vector := new Generic_Package_Vector_Type (1 .. Vector_Size);
      end Allocate_Vector;

      ----------------------------------------------------------------
      procedure Delete_All is
      ----------------------------------------------------------------

      begin
         for Object of Vector.all loop
            Free (Object);
         end loop;
      end Delete_All;

      ----------------------------------------------------------------
      procedure Dump (
         Object               : in     Generic_Package_Type;
         What                 : in     String := "";
         From                 : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         Video_Type (Object).Dump (What, From);
      end Dump;

      ----------------------------------------------------------------
      procedure Dump (
         What                 : in     String := "";
         From                 : in     String := Ada_Lib.Trace.Here) is
      ----------------------------------------------------------------

      begin
         for Object of Vector.all loop
            Object.Dump (What, From);
         end loop;
      end Dump;

      ----------------------------------------------------------------
      function Get_Constant_Object (
         Vector_Index        : in     Vector_Index_Type
      ) return Generic_Package_Constant_Class_Access is
      ----------------------------------------------------------------

      begin
         return Generic_Package_Constant_Class_Access (
            Vector.all (Vector_Index));
      end Get_Constant_Object;

      ----------------------------------------------------------------
      function Get_Object (
         Vector_Index           : in     Vector_Index_Type
      ) return Generic_Package_Class_Access is
      ----------------------------------------------------------------

      begin
         return Vector.all (Vector_Index);
      end Get_Object;

      ----------------------------------------------------------------
      function Get_Vector_Size
      return Vector_Index_Type is
      ----------------------------------------------------------------

      begin
         return Vector.all'last;
      end Get_Vector_Size;

      ---------------------------------------------------------------
      function Has_Object (
         Vector_Index        : in     Vector_Index_Type
      ) return Boolean is
      ---------------------------------------------------------------

         Vector_Size   : constant Vector_Index_Type := Vector.all'last;
      begin
         return Log_Pre_Post (Vector_Index <= Vector_Size,
            Debug, "Vector_Index" & Vector_Index'img &
            " vector size" & Vector_Size'img);
      end Has_Object;

      ----------------------------------------------------------------
      function Has_Vector
      return Boolean is
      ----------------------------------------------------------------

      begin
         return Vector /= Null;
      end Has_Vector;

      ----------------------------------------------------------------
      procedure Set_Object (
         Vector_Index      : in     Vector_Index_Type;
         Pointer           : in     Generic_Package_Class_Access) is
      ----------------------------------------------------------------

      begin
         Vector (Vector_Index) := Pointer;
      end Set_Object;

   begin
--Debug := True;
      Log_Here (Debug or Elaborate);
   end One_Dimension_Vector;

   ----------------------------------------------------------------
   package body Two_Dimensions_Vector is

      type Vector_Access is access Generic_Package_First_Vector_Type;

      Vector           : Vector_Access := Null;

      ----------------------------------------------------------------
      procedure Allocate_Vector (
         First_Size     : in     First_Vector_Index_Type;
         Second_Size    : in     Second_Vector_Index_Type) is
      ----------------------------------------------------------------

      begin
         Vector := new Generic_Package_First_Vector_Type (1 .. First_Size);
         for Index in 1 .. First_Size loop
            Vector (Index) := new Generic_Package_Second_Vector_Type (1 ..
               Second_Size);
         end loop;
      end Allocate_Vector;

      ----------------------------------------------------------------
      function Get_Constant_Second_Vector (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Generic_Package_Constant_Class_Access is
      ----------------------------------------------------------------

      begin
         return Generic_Package_Constant_Class_Access (
            Vector.all (First_Index).all (Second_Index));
      end Get_Constant_Second_Vector;

      ----------------------------------------------------------------
      function Get_Object (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Generic_Package_Class_Access is
      ----------------------------------------------------------------

      begin
         return Vector.all (First_Index).all (Second_Index);
      end Get_Object;

      ----------------------------------------------------------------
      function Get_First_Vector_Size
      return First_Vector_Index_Type is
      ----------------------------------------------------------------

      begin
         return Vector.all'last;
      end Get_First_Vector_Size;

      ---------------------------------------------------------------
      function Get_Second_Vector_Size (
         First_Index          : in     First_Vector_Index_Type
      ) return Second_Vector_Index_Type is
      ---------------------------------------------------------------

      begin
         return Vector.all (First_Index)'last;
      end Get_Second_Vector_Size;

      ---------------------------------------------------------------
      function Has_Vector (
         First_Index          : in     First_Vector_Index_Type
      ) return Boolean is
      ---------------------------------------------------------------

         Vector_Size   : constant First_Vector_Index_Type := Vector.all'last;

      begin
         return Log_Pre_Post (First_Index <= Vector_Size,
            Debug, "First_Index" & First_Index'img &
            " vector size" & Vector_Size'img);
      end Has_Vector;

      ---------------------------------------------------------------
      function Has_Vector (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Boolean is
      ---------------------------------------------------------------

         First_Size           : constant First_Vector_Index_Type :=
                                 Vector.all'last;
         Second_Size          : constant Second_Vector_Index_Type :=
                                 Vector.all (First_Index).all'last;

      begin
         return Log_Pre_Post (Second_Index <= Second_Size, Debug,
            "First_Size" & First_Size'img &
            " Second_Size" & Second_Size'img &
            " First_Index" & First_Index'img &
            " Second_Index" & Second_Index'img);
      end Has_Vector;

      ----------------------------------------------------------------
      function Has_Vector
      return Boolean is
      ----------------------------------------------------------------

      begin
         return Vector /= Null;
      end Has_Vector;

   begin
--Debug := True;
      Log_Here (Debug or Elaborate);
   end Two_Dimensions_Vector;

end Video.Generic_Package;

