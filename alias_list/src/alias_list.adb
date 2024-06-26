with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with ADA_LIB.Mail.CURL;
with ADA_LIB.Parser;
with ADA_LIB.Strings.Unlimited;
with ADA_LIB.Timer;
with ADA_LIB.Trace; use ADA_LIB.Trace;
with ADA_LIB.Trace_Tasks;
with GNATCOLL.Templates;
with Runtime_Options;

procedure Alias_List is

-- Failed                        : exception;
   CR_NL                         : constant String := Ada.Characters.Latin_1.CR &
                                    Ada.Characters.Latin_1.LF;
   Debug                         : Boolean renames Runtime_Options.Options.Debug;
   Email_Password                : constant String := "nvvjhdzbfthfwyey";
   Account                       : constant String := "wbullaughey@gmail.com";
   From_Address                  : constant String := "administrator@ucwc-pa.org";
   Options                       : Runtime_Options.Options_Constant_Class_Access renames
                                    Runtime_Options.Options;
   Template_Path                 : constant String := "data/alias_message";
   Parameter_Path                : constant String := "data/alias_list.csv";
   Parameters                    : GNATCOLL.Templates.Substitution_Array (1 .. 2);
   Source                        : ADA_LIB.Strings.Unlimited.String_Type;
   Source_File                   : Ada.Text_IO.File_Type;

begin
   Runtime_Options.Process;
   Put_Line ("email alias list" & Quote (" template", Template_Path) &
      Quote (" parameters", Parameter_Path));

   Parameters (1).Name := new String'("alias");
   Parameters (2).Name := new String'("forward");

   Ada.Text_IO.Open (Source_File, Ada.Text_IO.In_File, (if Options.Message_Path.Length > 0 then
          Options.Message_Path.Coerce
      else
         Template_Path));

   while not Ada.Text_IO.End_Of_File (Source_File) loop
      declare
         Buffer               : String (1 .. 512);
         Last                 : Natural;

      begin
         Ada.Text_IO.Get_Line (Source_File, Buffer, Last);
         Log_Here (Debug, Quote ("buffer", Buffer (Buffer'first .. Last)));
         Source.Append (Buffer (Buffer'first .. Last) & CR_NL);
      end;
   end loop;

   Ada.Text_IO.Close (Source_File);

   declare
      Email_Credentuals          : ADA_LIB.Mail.CURL.CURL_Credentials_Type;
      Parameter_File             : Ada.Text_IO.File_Type;

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

      Ada.Text_IO.Open (Parameter_File, Ada.Text_IO.In_File, (if Options.Parameter_Path.Length > 0 then
             Options.Parameter_Path.Coerce
         else
            Parameter_Path));

      while not Ada.Text_IO.End_Of_File (Parameter_File) loop
         declare
            Buffer               : String (1 .. 512);
            Last                 : Natural;

         begin
            Ada.Text_IO.Get_Line (Parameter_File, Buffer, Last);
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
                     Alias       : aliased String := Iterator.Get_Value (Do_Next => True);

                  begin
                     Log_Here (Debug, Quote ("alias", Alias));

                     if Alias /= "User Name" then
                        while not Iterator.At_End loop
                           declare
                              Forward     : aliased String := Iterator.Get_Value (Do_Next => True);

                           begin
                              Log_Here (Debug, Quote ("Forward", Forward));
                              Parameters (1).Value := Alias'unchecked_access;
                              Parameters (2).Value := Forward'unchecked_access;

                              declare
                                 Message        : constant String :=
                                                   GNATCOLL.Templates.Substitute (
                                                      Str         => Source.Coerce,
                                                      Substrings  => Parameters);
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

      Ada.Text_IO.Close (Parameter_File);

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
