--with AUnit.Test_Filters;
with ADA_LIB.Command_Line_Iterator;
--with ADA_LIB.Database.Unit_Test;
with ADA_LIB.Options;
with ADA_LIB.Strings.Unlimited;
with ADA_LIB.Trace;
--with ADA_LIB.Unit_Test.Options;
--with ADA_LIB.Unit_Test.GNOGA_Tests;
--with Runtime_Base_Options;

package Runtime_Options is
-- pragma elaborate_body;

   type Protocol_Type            is (CURL, No_Protocol, SMTP);

   type Options_Type is new ADA_LIB.Options.Options_Type with record
      Debug                      : Boolean;
      Message_Path               : ADA_LIB.Strings.Unlimited.String_Type;
      Parameter_Path             : ADA_LIB.Strings.Unlimited.String_Type;
      Password                   : ADA_LIB.Strings.Unlimited.String_Type;
      Protocol                   : Protocol_Type := No_Protocol;
      User                       : ADA_LIB.Strings.Unlimited.String_Type;
   end record;

   type Options_Class_Access is access all Options_Type'class;
   type Options_Constant_Class_Access is access constant Options_Type'class;

   procedure Initialize;

   function Get_Modifiable_Options (
      From                       : in  String := ADA_LIB.Trace.Here
   ) return Options_Class_Access;

   function Get_Options (
      From                       : in  String := ADA_LIB.Trace.Here
   ) return Options_Constant_Class_Access;

   procedure Process;

   overriding
   function Process_Option (  -- process one option
     Options                    : in out Options_Type;
     Iterator                   : in out ADA_LIB.Command_Line_Iterator.Abstract_Package.Iterator_Type'class;
     Option                     : in     Character
   ) return Boolean;

   Debug                         : aliased Boolean := False;  -- not set as option
   Options                       : Options_Constant_Class_Access := Null;

private

-- overriding
   procedure Process (     -- processes whole command line calling Process_Option for each option
     Options                    : in out Options_Type;
     Iterator                   : in out ADA_LIB.Command_Line_Iterator.
                                    Abstract_Package.Iterator_Type'class);

   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Runtime_Options;
