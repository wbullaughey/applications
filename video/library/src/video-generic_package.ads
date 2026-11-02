with Ada_Lib.Trace;

package Video.Generic_Package is

   generic

      type Vector_Index_Type  is new Video_ID_Type;

      type Base_Type          is abstract new Video_Type with private;
--    type Base_Class_Access  is access Base_Type'class;

      Debug                   : in     Boolean;

   package One_Dimension_Vector is

      Video_ID_Not_Set        : constant Vector_Index_Type :=
                                 Vector_Index_Type'first;

      type Generic_Package_Type
                              is new Base_Type with null record;
      type Generic_Package_Access
                              is access all Generic_Package_Type;
      type Generic_Package_Class_Access
                              is access all Generic_Package_Type'class;
      type Generic_Package_Constant_Class_Access
                              is access constant Generic_Package_Type'class;
      type Generic_Package_Vector_Type
                              is array (Vector_Index_Type range <>)
                                 of Generic_Package_Class_Access;

      procedure Allocate_Vector (
         Vector_Size   : in     Vector_Index_Type
      ) with Pre  => not Has_Vector,
             Post => Has_Vector;

      procedure Delete_All
      with Pre => Has_Vector;

      procedure Dump (
         What                 : in     String := "";
         From                 : in     String := Ada_Lib.Trace.Here
      ) with Pre => Has_Vector;

      procedure Dump (
         Object               : in     Generic_Package_Type;
         What                 : in     String := "";
         From                 : in     String := Ada_Lib.Trace.Here);

      function Get_Constant_Object (
         Vector_Index        : in     Vector_Index_Type
      ) return Generic_Package_Constant_Class_Access
      with Pre    => Has_Object (Vector_Index);

      function Get_Object (
         Vector_Index        : in     Vector_Index_Type
      ) return Generic_Package_Class_Access
      with Pre    => Has_Object (Vector_Index);

      function Get_Vector_Size
      return Vector_Index_Type
      with Pre    => Has_Vector;

      function Has_Vector
      return Boolean;

      function Has_Object (
         Vector_Index        : in     Vector_Index_Type
      ) return Boolean
      with Pre => Has_Vector;

      procedure Set_Object (
         Vector_Index      : in     Vector_Index_Type;
         Pointer           : in     Generic_Package_Class_Access
      ) with Pre => Has_Vector;

   end One_Dimension_Vector;

   generic

      type First_Vector_Index_Type  is new Video_ID_Type;
      type Second_Vector_Index_Type is new Video_ID_Type;

      type Base_Type          is abstract tagged private;
--    type Base_Class_Access  is access Base_Type'class;

      Debug                   : in     Boolean;

   package Two_Dimensions_Vector is

      Video_ID_Not_Set        : constant First_Vector_Index_Type :=
                                 First_Vector_Index_Type'first;

      type Generic_Package_Type
                              is abstract new Base_Type with null record;
      type Generic_Package_Access
                              is access all Generic_Package_Type;
      type Generic_Package_Class_Access
                              is access all Generic_Package_Type'class;
      type Generic_Package_Constant_Class_Access
                              is access constant Generic_Package_Type'class;
      type Generic_Package_Second_Vector_Type
                              is array (Second_Vector_Index_Type range <>)
                                 of Generic_Package_Class_Access;
      type Generic_Package_Second_Vector_Access
                              is access all Generic_Package_Second_Vector_Type;
      type Generic_Package_Second_Vector_Constant_Access
                        is access constant Generic_Package_Second_Vector_Type;
      type Generic_Package_First_Vector_Type
                        is array (First_Vector_Index_Type range <>)
                           of Generic_Package_Second_Vector_Access;
      type Generic_Package_First_Vector_Access
                              is access all Generic_Package_First_Vector_Type;

      procedure Allocate_Vector (
         First_Size     : in     First_Vector_Index_Type;
         Second_Size    : in     Second_Vector_Index_Type
      ) with Pre  => not Has_Vector,
             Post => Has_Vector;

      procedure Dump (
         Generic_Package      : in     Generic_Package_Type;
         What                 : in     String := "";
         From                 : in     String := Ada_Lib.Trace.Here) is abstract;

      function Get_Constant_Second_Vector (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Generic_Package_Constant_Class_Access
      with Pre    => Has_Vector (First_Index);

      function Get_Object (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Generic_Package_Class_Access
      with Pre    => Has_Vector (First_Index);

      function Get_First_Vector_Size
      return First_Vector_Index_Type
      with Pre    => Has_Vector;

      function Get_Second_Vector_Size (
         First_Index          : in     First_Vector_Index_Type
      ) return Second_Vector_Index_Type
      with Pre    => Has_Vector (First_Index);

      function Has_Vector
      return Boolean;

      function Has_Vector (
         First_Index          : in     First_Vector_Index_Type
      ) return Boolean
      with Pre => Has_Vector;

      function Has_Vector (
         First_Index          : in     First_Vector_Index_Type;
         Second_Index         : in     Second_Vector_Index_Type
      ) return Boolean
      with Pre => Has_Vector (First_Index);

   end Two_Dimensions_Vector;

end Video.Generic_Package;
