with Configuration;
with Video;

package Generics is

   generic

      type Result_Type           is new Positive;

      with function Accessor (
         Configuration           : in     Standard.Configuration.Abstract_Configuration_Type'class
      ) return Result_Type;

   function Get_Default_Value (
      Video_ID                   : in     Video.Video_ID_Type
   ) return Result_Type;

   generic

      type ID_Type               is mod <>;

      type Match_Type            is private;

      type Object_Type           is tagged private;

      type Object_Class_Access   is access Object_Type'class;

      type Object_Constant_Class_Access
                                 is access constant Object_Type'class;

      type Vector_Type           is array (ID_Type range <>) of
                                    Object_Class_Access;

      type Vectors_Type          is access Vector_Type;

   package Vector_Package is

      type Callback_Access is access function (
         Object                  : in     Object_Constant_Class_Access;
         Match                   : in     Match_Type
      ) return Boolean;

      procedure Allocate_Object (
         ID                      : in     ID_Type;
         Object                  : Object_Class_Access
      ) with Pre => not Has_Object (ID);

      procedure Allocate_Vector (
         Max_ID                  : in     ID_Type
      ) with Pre => not Has_Vector;

      procedure Free_Vector
      with Pre => Has_Vector;

      function Get (
         ID                      : in     ID_Type
      ) return Object_Class_Access
      with Pre => Has_Object (ID);

      function Get_Constant (
         ID                      : in     ID_Type
      ) return Object_Constant_Class_Access
      with Pre => Has_Object (ID);


      function Has_Object (
         ID                      : in     ID_Type
      ) return Boolean
      with Pre => Has_Vector;

      function Has_Vector
      return Boolean;

      function Iterate (
         Callback                : in     Callback_Access;
         Match                   : in     Match_Type
      ) return Object_Class_Access;

      procedure Set_Object (
         ID                      : in     ID_Type;
         Object                  : in     Object_Class_Access
      ) with Pre => not Has_Object (ID);

   end Vector_Package;

end Generics;
