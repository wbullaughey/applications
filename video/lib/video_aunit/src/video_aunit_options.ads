with Ada_Lib.Options;
with Ada_Lib.Options.Unit_Test;
with Ada_Lib.Options;
with Ada_Lib.Options;
with Gnoga.Gui.Base;
with Video.Lib;

package Runtime_Options is

   Failed                        : exception;

   subtype Runtime_Iterator_Type is Ada_Lib.Command_Line_Iterator.
                                    Abstract_Package.Abstract_Iterator_Type;

   type Source_Iterator_Type     is new Ada_Lib.Command_Line_Iterator.
                                    Internal.Iterator_Type with record
      Window                     : Gnoga.Gui.Base.Pointer_To_Base_Class;
   end record;

   type Options_Type is limited new Video.Lib.Options_Type with record
      Unit_Test                  : Ada_Lib.Options.Unit_Test.
                                    Camera_Lib_Unit_Test_Options_Type (True);
   end record;

   type Options_Access           is access all Options_Type;
   type Options_Class_Access     is access all Options_Type'class;
   type Options_Constant_Class_Access
                                 is access constant Options_Type'class;

   function Get_Modifiable_Options return Options_Access;

   overriding
   function Initialize (
     Options                     : in out Options_Type
   ) return Boolean
   with pre => Options.Verify_Preinitialize;

   procedure Initialize;

   overriding
   function Process_Option (  -- process one option
      Options           : in out Options_Type;
      Iterator          : in out Ada_Lib.Options.
                                    Command_Line_Iterator_Interface'class;
      Option            : in     Ada_Lib.Options.Option_Type'class
   ) return Boolean
   with pre => Options.Initialized;

   Debug                         : aliased Boolean := False;  -- not set as option
   Options                       : Options_Constant_Class_Access := Null;

private

   overriding
   procedure Program_Help (
      Options                    : in     Options_Type;  -- only used for dispatch
      Help_Mode                  : in     ADA_LIB.Options.Help_Mode_Type);

end Runtime_Options;
