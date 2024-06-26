with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
--with ADA_LIB.Mail.CURL.GMail;
with ADA_LIB.Mail.SMTP;
with ADA_LIB.Parser;
with ADA_LIB.Strings.Unlimited;
with ADA_LIB.Template.Compile;
with ADA_LIB.Template.Parameters;
with ADA_LIB.Timer;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with ADA_LIB.Trace_Tasks;
--with Hex_IO;
with Runtime_Options;

procedure Alias_List is

-- Failed                        : exception;
   Debug                         : Boolean renames Runtime_Options.Options.Debug;
   Email_Password                : constant String := "nvvjhdzbfthfwyey";
   Account                       : constant String := "wbullaughey@gmail.com";
   From_Address                  : constant String := "administrator@ucwc-pa.org";
   Max_Parameters                : constant := 2;
   Options                       : Runtime_Options.Options_Constant_Class_Access renames
                                    Runtime_Options.Options;
   Template_Path                 : constant String := "data/alias_message";
   Parameter_Path                : constant String := "data/alias_list.csv";


begin
   Runtime_Options.Process;
   Put_Line ("email alias list" & Quote (" template", Template_Path) &
      Quote (" parameters", Parameter_Path));

   declare
      Email_Credentuals          : ADA_LIB.Mail.SMTP.SMTP_Credentials_Type;
      Input                      : Ada.Text_IO.File_Type;
      Template_Source            : constant String := ADA_LIB.Template.Compile.Load (
         if Options.Message_Path.Length > 0 then
             Options.Message_Path.Coerce
         else
            Template_Path);

   begin
      Email_Credentuals.Initialize (
         (if Options.User.Length = 0 then
            Account
         else
            Options.User.Coerce),
         (if Options.Password.Length = 0 then
            Email_Password
         else
            Options.Password.Coerce),
         From_Address);

      Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, (if Options.Parameter_Path.Length > 0 then
             Options.Parameter_Path.Coerce
         else
            Parameter_Path));

      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Buffer               : String (1 .. 512);
            Last                 : Natural;

         begin
            Ada.Text_IO.Get_Line (Input, Buffer, Last);
            Log_Here (Debug, Quote ("buffer", Buffer (Buffer'first .. Last)) &
               " last - 1 " & character'pos (Buffer (Last - 1))'img &
               " last " & character'pos (Buffer (Last))'img);

            declare
               Iterator          : ADA_LIB.Parser.Iterator_Type :=
                                    ADA_LIB.Parser.Initialize (
                                       Seperators     => ",",
                                       Quotes         => """",
                                       Value          => Buffer (Buffer'first .. Last));

            begin
               while not Iterator.At_End loop
                  declare
                     Alias       : constant String := Iterator.Get_Value (Do_Next => True);

                  begin
                     Log_Here (Debug, Quote ("alias", Alias));

                     if Alias /= "User Name" then
                        while not Iterator.At_End loop
                           declare
                              Parameters  : ADA_LIB.Template.Parameters.Parameter_Array (1 .. Max_Parameters);
                              Forward     : constant String := Iterator.Get_Value (Do_Next => True);

                           begin
                              Log_Here (Debug, Quote ("Forward", Forward));
                              Parameters (1) := new ADA_LIB.Template.Parameters.String_Parameter_Type'(
                                 Name     => ADA_LIB.Strings.Unlimited.Coerce ("alias"),
                                 Value    => ADA_LIB.Strings.Unlimited.Coerce (Alias));
                              Parameters (2) := new ADA_LIB.Template.Parameters.String_Parameter_Type'(
                                 Name     => ADA_LIB.Strings.Unlimited.Coerce ("forward"),
                                 Value    => ADA_LIB.Strings.Unlimited.Coerce (Forward));

                              declare
                                 Template       : ADA_LIB.Template.Compile.Template_Type;
                                 Message        : constant String :=
                                                   Template.Compile (Template_Source, Parameters);
                              begin
                                 Log_Here (Debug, Quote ("message", Message));
                                 Put_Line ("sending message:");
                                 Put_Line (Message);
                                 Email_Credentuals.Send (Forward, Message, "UCWC alias list update",
                                    Runtime_Options.Options.Verbose);
                                 Put_Line ("message sent");
                              end;
                           end;
                        end loop;
                     end if;
                  end;
               end loop;

            end;
         end;
      end loop;

      Ada.Text_IO.Close (Input);

log_here;
      if not ADA_LIB.Trace_Tasks.All_Stopped then
         ADA_LIB.Trace_Tasks.Report;
      end if;

      ADA_LIB.Timer.Stop;
      Log_Out (Debug);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Put_Line (Quote ("could not open parameter file ", Parameter_Path));

      when Fault: others =>
         Trace_Exception (Debug, Fault);
         Put_Line ("exception: " &Ada.Exceptions.Exception_Name (Fault));
         Put_Line ("message: " & Ada.Exceptions.Exception_Message (Fault));

   end;
end Alias_List;
