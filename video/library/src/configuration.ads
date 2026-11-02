with Ada_Lib.Socket_IO;
with Ada_Lib.Trace;
with Video;

package Configuration is

   subtype Address_Kind_Type  is Ada_Lib.Socket_IO.Address_Kind_Type;

   type Column_Type           is new Positive;
   type Base_Configuration_ID_Type
                              is new Video.Data_Type;
   type Pan_Type              is new Positive;
   type Row_Type              is new Positive;
   type Speed_Type            is new Positive;
   type Tilt_Type             is new Positive;
   type Zoom_Type             is new Positive;

   Configuration_Not_Set      : constant := Base_Configuration_ID_Type'last;
   IP                         : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.IP;
   NOT_SET                    : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.NOT_SET;
   URL                        : Address_Kind_Type renames
                                 Ada_Lib.Socket_IO.URL;

   type Abstract_Configuration_Type    is abstract tagged private;
   type Abstract_Configuration_Access  is access all Abstract_Configuration_Type;
   type Abstract_Configuration_Constant_Access
                              is access constant Abstract_Configuration_Type;
   type Abstract_Configuration_Class_Access
                              is access all Abstract_Configuration_Type'class;
   type Abstract_Configuration_Constant_Class_Access
                              is access constant Abstract_Configuration_Type'class;

   procedure Dump (
      Setup          : in     Abstract_Configuration_Type;
      What           : in     String := "";
      From           : in     String := Ada_Lib.Trace.Here) is abstract;

   function Is_Loaded (
      Configuration           : in     Abstract_Configuration_Type;
      From                    : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   procedure Set_Loaded (
      Configuration           : in out Abstract_Configuration_Type;
      Value                   : in     Boolean;
      From                    : in     String := Ada_Lib.Trace.Here
   ) with Pre => Value /= Configuration.Is_Loaded,
          Post => Value = Configuration.Is_Loaded;

private

   type Abstract_Configuration_Type
                              is abstract tagged record
      Loaded                     : Boolean := False;
   end record;

end Configuration;
