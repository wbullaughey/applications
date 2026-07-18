with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_Lib.Strings; use Ada_Lib.Strings;
with Ada_Lib.Trace; use Ada_Lib.Trace;
with Camera.Base;
with Camera.Lib.Options;
with Camera.Main;
with Configuration.Camera.Setup;
with Configuration.Camera.State;

package body Camera.Configurations is

   use type Base.Configuration_Class_Access;
-- use type Configuration.Configuration_Access;
-- use type Configuration.Configuration_Class_Access;

   function Resolve_Camera_ID (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Camera_ID_Type;

   function State_Equal (
      Left, Right       : Base.Configuration_Class_Access
   ) return Boolean;

   package State_Package  is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type       => Camera_ID_Type,
      Element_Type   => Base.Configuration_Class_Access,
      Hash           => Camera_ID_Hash,
      Equivalent_Keys=> Camera_ID_Equal,
      "="            => State_Equal);

   use type State_Package.Cursor;

   Current_Camera_ID : Camera_ID_Type := Null_Camera_ID;
   Debug             : Boolean renames
                        Lib.Options.Configuration_Options.Camera_Debug;
   States            : State_Package.Map;

   ----------------------------------------------------------------
   function Allocate_Window_Connection (
      Camera_ID            : in        Camera_ID_Type
   ) return Camera_Main_Window_Connection_Class_Access is
   ----------------------------------------------------------------

--    Camera_State         : constant Configuration.Configuration_Access :=
--                            Allocate_State (Camera_ID);

   begin
      Log_In (Debug, "ID:" &Camera_ID'img);
not_implemented;
--    if Camera_State.Window_Connection = Null then
--       Log_Here ("debug new camera state");
--       Camera_State.Window_Connection := Camera_Main_Window_Connection_Class_Access (
--          Main.Allocate_Window_Connection);
--    else
--       Log_Here ("debug current camera state");
--    end if;
--
--    Log_Out (Debug);
--    return Camera_State.Window_Connection;
return null;
   end Allocate_Window_Connection;

   ----------------------------------------------------------------
   procedure Clear_Configuration is
   ----------------------------------------------------------------

      -------------------------------------------------------------
      procedure Process (
         Position       : in  State_Package.Cursor) is
      -------------------------------------------------------------

         Configuration  : constant Base.Configuration_Class_Access :=
                           State_Package.Reference (States, Position);
      begin
         Log_Here (Debug,
            Tag_Name ("configuration tag", Configuration.all'tag) &
            " address " & Ada_Lib.Strings.Image (Configuration.all'address));
         Configuration.Deallocate;
      end Process;
      -------------------------------------------------------------

   begin
      Log_In (Debug);
      State_Package.Iterate (States, Process'access);
      State_Package.Clear (States);
      Log_Out (Debug);
   end Clear_Configuration;

   ----------------------------------------------------------------
   function Get_Camera_Configuration_Setup (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_Setup_Class_Access is
   ----------------------------------------------------------------

   begin
      return Get_Configuration (Camera_ID).Get_Configuration_Setup;
   end Get_Camera_Configuration_Setup;

   ----------------------------------------------------------------
   function Get_Camera_Configuration_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_State_Class_Access is
   ----------------------------------------------------------------

   begin
      return Get_Configuration (Camera_ID).Get_Configuration_State;
   end Get_Camera_Configuration_State;

   ----------------------------------------------------------------
   function Get_Camera_Names
   return Camera_Names_Type is
   ----------------------------------------------------------------

      Index       : Natural := 0;
      Result      : Camera_Names_Type (1 .. Positive (States.Length));

   begin
      for State of States loop
         Index := Index + 1;
         Result (Index).Construct (State.Get_Camera_Name);
      end loop;

      return Result;
   end Get_Camera_Names;

   ----------------------------------------------------------------
   function Get_Configuration (
      Camera_ID   : in     Camera_ID_Type := Null_Camera_ID
   ) return Base.Configuration_Class_Access is
   ----------------------------------------------------------------

   begin
      Log_Here (Debug, "camera id " & Camera_ID.Image);

      declare
         Configuration  : constant Base.Configuration_Class_Access :=
                           State_Package.Element (States,
                           Resolve_Camera_ID (if Camera_ID = Null_Camera_ID then
                              Current_Camera_ID
                           else
                              Camera_ID));
      begin
         return Configuration;
      end;
   end Get_Configuration;

-- ----------------------------------------------------------------
   function Get_Read_Only_Camera_Configuration_Setup (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_Setup_Constant_Access is
   ----------------------------------------------------------------

      Configuration   : constant Camera.Configuration.
                        Configuration_Constant_Access :=
                           Get_Read_Only_Configuration (Camera_ID);
   begin
      return Camera_Configuration_Setup_Constant_Access (
         Configuration.Get_Configuration_Setup);
   end Get_Read_Only_Camera_Configuration_Setup;

-- ----------------------------------------------------------------
   function Get_Read_Only_Camera_Configuration_State (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Camera_Configuration_State_Constant_Class_Access is
   ----------------------------------------------------------------

   begin
      return Camera_Configuration_State_Constant_Class_Access (
         Get_Camera_Configuration_State (Camera_ID));
   end Get_Read_Only_Camera_Configuration_State;

   ----------------------------------------------------------------
   function Get_Read_Only_Configuration (
      Camera_ID   : Camera_ID_Type := Null_Camera_ID
   ) return Configuration.Configuration_Constant_Access is
   ----------------------------------------------------------------

   begin
      return Configuration.Configuration_Constant_Access (
         Get_Configuration (Camera_ID));
   end Get_Read_Only_Configuration;

     ----------------------------------------------------------------
     function Has_Camera_Configuration_Setup (
        Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
     ) return Boolean is
     ----------------------------------------------------------------

        Configuration         : constant Base.Configuration_Class_Access :=
                                 Get_Configuration (Camera_ID);
        Result   : constant Boolean := Configuration.Has_Configuration_Setup;

     begin
        return Log_Here (Result, Trace_Pre_Post (Result, Debug));
     end Has_Camera_Configuration_Setup;

     ----------------------------------------------------------------
     function Has_Camera_Configuration_State (
        Camera_ID          : in        Camera_ID_Type := Null_Camera_ID
     ) return Boolean is
     ----------------------------------------------------------------

        Configuration      : constant Base.Configuration_Class_Access :=
                              Get_Configuration (Camera_ID);
        Result   : constant Boolean := Configuration.Has_Configuration;

     begin
        return Log_Here (Result, Trace_Pre_Post (Result, Debug));
     end Has_Camera_Configuration_State;

     ----------------------------------------------------------------
     function Has_Camera_ID(
        Camera_ID    : in        Camera_ID_Type := Null_Camera_ID
     ) return Boolean is
     ----------------------------------------------------------------

         Result      : constant Boolean :=
                        Camera_ID.Set or else Current_Camera_ID.Set;
     begin
        return Log_Here (Result, Trace_Pre_Post (Result, Debug),
           Camera_ID.Image & Current_Camera_ID.Image);
     end Has_Camera_ID;

   ----------------------------------------------------------------
   function Has_Configuration (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Boolean is
   ----------------------------------------------------------------

      Check_Camera_ID   : constant Camera_ID_Type := Resolve_Camera_ID (
                              Camera_ID);
      Result            : constant Boolean :=
                           State_Package.Find (States, Check_Camera_ID) /=
                              State_Package.No_Element;
   begin
      return Log_Here (Result, Trace_Pre_Post (Result, Debug),
         "using " & Check_Camera_ID.Image &
         (if Result then
            ""
         else
            " not") &
         " allocated");
   end Has_Configuration;
--
  ----------------------------------------------------------------
   function Resolve_Camera_ID (
      Camera_ID            : in        Camera_ID_Type := Null_Camera_ID
   ) return Camera_ID_Type is
  ----------------------------------------------------------------

      Camera_ID_Set     : constant Boolean := Camera_ID /= Null_Camera_ID;
      Check_Camera_ID   : constant Camera_ID_Type := (
                           if Camera_ID_Set then
                              Camera_ID
                           else
                              Current_Camera_ID);
   begin
      Log_Here (Debug, " Current_Camera_ID " & Current_Camera_ID.Image &
         " Camera_ID_Set " & Camera_ID_Set'img &
         " camera id " & Camera_ID.Image &
         " check camera id " & Check_Camera_ID.Image &
         " Null_Camera_ID " & Null_Camera_ID.Image);
      return Check_Camera_ID;
   end Resolve_Camera_ID;

------------------------------------------------------------------
--procedure Set_Current_Camera_ID (
--   Camera_ID   : in     Camera_ID_Type) is
------------------------------------------------------------------
--
--begin
--   Log_Here (Debug, "Camera_ID " & Camera_ID.Image);
--   Current_Camera_ID := Camera_ID;
--end Set_Current_Camera_ID;

   ----------------------------------------------------------------
   procedure Set_State (
      Camera_ID      : in     Camera_ID_Type;
      Configuration  : access Base.Configuration_Type'class) is
   ----------------------------------------------------------------

   begin
      Log_In (Debug, "ID:" & Camera_ID.Image &
         Tag_Name ("configuration tag", Configuration'tag) &
         " address " & Ada_Lib.Strings.Image (Configuration'address));
      State_Package.Insert (States, Camera_ID,
         Base.Configuration_Class_Access (Configuration));
      Current_Camera_ID := Camera_ID;
      Log_Out (Debug, "state set");
   end Set_State;

   ----------------------------------------------------------------
   function State_Equal (
      Left, Right       : Base.Configuration_Class_Access
   ) return Boolean is
   ----------------------------------------------------------------

   begin
      return Left = Right;
   end State_Equal;

begin
--Debug := True;
   Log_Here (Debug);
end Camera.Configurations;
