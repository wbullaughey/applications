with ADA_LIB.Trace; use ADA_LIB.Trace;

package body Camera is

   ---------------------------------------------------------------
   function Convert (
      Absolute                   : in     Absolute_Type
   ) return Value_Type is
   ---------------------------------------------------------------

      Conversion                 : Interfaces.Unsigned_16;
      for Conversion'address use Absolute'address;
      Result                     : constant Value_Type :=
                                    Value_Type (Conversion);

   begin
      Log_Here (Debug, "Absolute" & Absolute'img & " result" & Result'img);
      return Result;
   end Convert;

   ---------------------------------------------------------------
   function Convert (
      Relative                   : in     Relative_Type
   ) return Value_Type is
   ---------------------------------------------------------------

      Conversion                 : Interfaces.Unsigned_16;
      for Conversion'address use Relative'address;
      Result                     : constant Value_Type :=
                                    Value_Type (Conversion);

   begin
      Log_Here (Debug, "Relative" & Relative'img & " result" & Result'img);
      return Result;
   end Convert;

end Camera;
