with ADA_LIB.Trace; use ADA_LIB.Trace;

package body Simulator is

   task Simulator_Task is

      entry Start;
      entry Stop;

   end Simulator_Task;

   ---------------------------------------------------------------
   procedure Start is
   ---------------------------------------------------------------


   begin
      Log_In (Debug);
      Simulator_Task.Start;
      Log_Out (Debug);
   end Start;

   ---------------------------------------------------------------
   procedure Stop is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      Simulator_Task.Stop;
      Log_Out (Debug);
   end Stop;

   ---------------------------------------------------------------
   task body Simulator_Task is
   ---------------------------------------------------------------

   begin
      Log_In (Debug);
      accept Start;
         Log_Here (Debug);

      accept Stop;
         Log_Here (Debug);

      Log_Out (Debug);
   end Simulator_Task;
end Simulator;
