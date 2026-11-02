with Ada_Lib.Socket_IO.Client;
with Ada_Lib.Trace;
with Hex_IO;
with Interfaces;

package Video is

   type Data_Type             is new Interfaces.Unsigned_32;
   subtype Index_Type         is Positive;
   type Buffer_Type           is array (Index_Type range <>) of Data_Type;
   type Video_ID_Type         is new Data_Type;
   type Video_Type            is abstract tagged private;

   procedure Buffer_Dump is new Hex_IO.Integer_Dump (Index_Type, Data_Type, Buffer_Type);

   procedure Dump (
      Video                   : in     Video_Type;
      What                    : in     String := "";
      From                    : in     String := Ada_Lib.Trace.Here);

   function Hex is new Hex_IO.Modular_Hex (Data_Type);

   function Is_Loaded (
      Video                   : in     Video_Type;
      From                    : in     String := Ada_Lib.Trace.Here
   ) return Boolean;

   procedure Set_Loaded (
      Video                   : in out Video_Type;
      Value                   : in     Boolean;
      From                    : in     String := Ada_Lib.Trace.Here
   ) with Pre => Value /= Video.Is_Loaded,
          Post => Value = Video.Is_Loaded;

   procedure Socket_Read (
      Socket         : in out Ada_Lib.Socket_IO.Client.Client_Socket_Type;
      Buffer         :    out Buffer_Type;
      Timeout_Length : in     Duration := Ada_Lib.Socket_IO.No_Timeout);

   procedure Unload (
      Video                   : in out Video_Type
   ) with Pre => Video.Is_Loaded;

private

   type Video_Type            is abstract tagged record
      Loaded                  : Boolean := False;
      Modified                : Boolean := False;
   end record;

end Video;