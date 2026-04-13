with AUnit.Test_Cases;

package body Video.Lib.Unit_Test is

   procedure Add_Optional_Routine (
      Test                 : in out Video_Lib_Test_Case_Type;
      Routine              : in     AUnit.Test_Cases.Test_Routine;
      Suite_Name           : in     String;
      Routine_Name         : in     String;
      Needs_Camera         : in     Boolean) is

   begin
      Nested_Options : constant Options.Unit_Test.
                           Ada_Lib_Unit_Test_Nested_Options_Constant_Class_Access :=
                        Options.Unit_Test.
                           Get_Readonly_Ada_Lib_Unit_Test_Nested_Options;
   begin
      Log_Here (Debug, "mode " & Nested_Options.Mode'img &
         " Needs_Camera " & Needs_Camera'img &
         " Has_Camera " & Video.Lib.Has_Camera'img);
      if Nested_Options.Mode /= Options.List_Suites and then
             Needs_Camera and then not Video.Lib.Has_Camera then
         Put_Line ("skipping " & Suite_Name & " routine " & Routine_Name);
      else
         Test_Case_Type (Test).Add_Routine (AUnit.Test_Cases.Routine_Spec'(
            Routine, AUnit.Format (Routine_Name)));
      end if;
   end Add_Optional_Routine;

end Video.Lib.Unit_Test;

