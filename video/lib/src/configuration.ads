with Ada_Lib.Socket_IO;

package Configuration is

   subtype Address_Kind_Type     is Ada_Lib.Socket_IO.Address_Kind_Type;

   IP                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.IP;
   NOT_SET                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.NOT_SET;
   URL                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.URL;
   type Base_Type                is tagged record
      Set                        : Boolean := False;
      Updated                    : Boolean := False;
   end record;

end Configuration;
