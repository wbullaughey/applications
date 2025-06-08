with Ada_Lib.Socket_IO;
with Ada_Lib.Trace;

package Configuration is

   subtype Address_Kind_Type     is Ada_Lib.Socket_IO.Address_Kind_Type;

   IP                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.IP;
   NOT_SET                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.NOT_SET;
   URL                            : Address_Kind_Type renames
                                    Ada_Lib.Socket_IO.URL;
   type Root_Setup_Type          is tagged private;

   function Is_Loaded (
      Setup                      : in     Root_Setup_Type
   ) return Boolean;

   procedure Set_Loaded (
      Setup                      :    out Root_Setup_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here
   ) with Pre => Value /= Setup.Is_Loaded,
          Post => Value = Setup.Is_Loaded;

   type Root_State_Type                is tagged private;

   function Is_Loaded (
      State                      : in     Root_State_Type
   ) return Boolean;

   procedure Set_Loaded (
      State                      :    out Root_State_Type;
      Value                      : in     Boolean;
      From                       : in     String := Ada_Lib.Trace.Here
   ) with Pre => Value /= State.Is_Loaded,
          Post => Value = State.Is_Loaded;

   Debug                         : Boolean := False;
   Initial_Root_Setup            : constant Root_Setup_Type;
   Initial_Root_State            : constant Root_State_Type;

private

   type Root_Setup_Type          is tagged record
      Loaded                     : Boolean := False;
   end record;

   type Root_State_Type                is tagged record
      Loaded                     : Boolean := False;
      Updated                    : Boolean := False;
   end record;

   Initial_Root_Setup            : constant Root_Setup_Type := (
      Loaded                     => False);

   Initial_Root_State            : constant Root_State_Type := (
      Loaded                     => False,
      Updated                    => False);

end Configuration;
