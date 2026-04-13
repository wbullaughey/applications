with AUnit.Test_Cases;

package Video.Lib.Unit_Test is

   type Video_Lib_Test_Case_Type is new Test_Case_Type null record;

   procedure Add_Optional_Routine (
      Test                 : in out Video_Lib_Test_Case_Type;
      Routine              : in     AUnit.Test_Cases.Test_Routine;
      Suite_Name           : in     String;
      Routine_Name         : in     String;
      Needs_Camera         : in     Boolean);

end Video.Lib.Unit_Test;

