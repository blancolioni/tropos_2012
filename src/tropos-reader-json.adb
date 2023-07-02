with Ada.Characters.Handling;
with Ada.Text_IO;

package body Tropos.Reader.Json is

   Max_Line_Length    : constant := 4000;
   Current_Line       : String (1 .. Max_Line_Length);
   Current_Line_Index : Natural;
   Current_Last       : Natural;
   Current_Index      : Natural;
   Done               : Boolean;

   procedure Start;
   procedure Finish;

   function End_Of_File return Boolean;
   function Current_Character return Character;
   procedure Next_Character;

   procedure Skip_Whitespace;

   procedure Error (Message : String);

   procedure Parse_Json_Element (Config : in out Configuration);
   procedure Parse_Json_Object (Config : in out Configuration);
   procedure Parse_Json_Array (Config : in out Configuration);

   function Parse_Terminal return String;
   function Parse_Rest_Of_String return String;

   -----------------------
   -- Current_Character --
   -----------------------

   function Current_Character return Character is
   begin
      if Done then
         return ' ';
      else
         return Current_Line (Current_Index);
      end if;
   end Current_Character;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File return Boolean is
   begin
      return Done;
   end End_Of_File;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Current_Line (1 .. Current_Last));
      for I in 1 .. Current_Index - 1 loop
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, '-');
      end loop;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "^");
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "line" & Current_Line_Index'Image
         & " column" & Current_Index'Image
         & ": " & Message);
      raise Constraint_Error with Message;
   end Error;

   ------------
   -- Finish --
   ------------

   procedure Finish is
   begin
      null;
   end Finish;

   --------------------
   -- Next_Character --
   --------------------

   procedure Next_Character is
   begin
      if Current_Index >= Current_Last then
         begin
            if Current_Last < Current_Line'Last then
               Current_Line_Index := Current_Line_Index + 1;
            end if;
            if Ada.Text_IO.End_Of_File then
               Done := True;
               return;
            end if;

            Ada.Text_IO.Get_Line (Current_Line, Current_Last);
         exception
            when Ada.Text_IO.End_Error =>
               Done := True;
               return;
         end;
         Current_Index := 0;
         Next_Character;
         return;
      end if;

      Current_Index := Current_Index + 1;
   end Next_Character;

   ----------------------
   -- Parse_Json_Array --
   ----------------------

   procedure Parse_Json_Array (Config : in out Configuration) is
      Index : Natural := 0;
   begin
      while not Done
        and then Current_Character /= ']'
      loop
         Index := Index + 1;
         declare
            Array_Config : Configuration :=
                             New_Config (Index);
         begin
            Parse_Json_Element (Array_Config);
            Config.Add (Array_Config);
         end;
         Skip_Whitespace;

         if Current_Character = ',' then
            Next_Character;
            Skip_Whitespace;
         elsif Current_Character /= ']' then
            Error ("missing ',' in array");
         end if;

      end loop;

      if Current_Character = ']' then
         Next_Character;
      else
         Error ("missing ']'");
      end if;
   end Parse_Json_Array;

   -----------------------
   -- Parse_Json_Config --
   -----------------------

   procedure Parse_Json_Config (Config : in out Configuration) is
   begin
      Start;

      while not End_Of_File loop
         Parse_Json_Element (Config);
      end loop;

      Finish;
   end Parse_Json_Config;

   ------------------------
   -- Parse_Json_Element --
   ------------------------

   procedure Parse_Json_Element (Config : in out Configuration) is
   begin
      Skip_Whitespace;

      if Done then
         return;
      end if;

      case Current_Character is
         when '{' =>
            Next_Character;
            Skip_Whitespace;
            Parse_Json_Object (Config);
         when '[' =>
            Next_Character;
            Skip_Whitespace;
            Parse_Json_Array (Config);
         when others =>
            declare
               Id : constant String := Parse_Terminal;
            begin
               Config.Add (New_Config (Id));
            end;
      end case;
   end Parse_Json_Element;

   -----------------------
   -- Parse_Json_Object --
   -----------------------

   procedure Parse_Json_Object (Config : in out Configuration) is
   begin
      while not Done
        and then Current_Character /= '}'
      loop
         case Current_Character is
            when '"' =>
               declare
                  Id : constant String := Parse_Terminal;
                  Child_Config : Configuration :=
                                   New_Config (Id);
               begin
                  if Current_Character = ':' then
                     Next_Character;
                  else
                     Error ("missing value");
                  end if;

                  Parse_Json_Element (Child_Config);
                  Config.Add (Child_Config);

               end;
            when others =>
               Error ("missing field name");
         end case;

         Skip_Whitespace;

         if Current_Character = ',' then
            Next_Character;
         elsif Current_Character = '}' then
            null;
         else
            Error ("missing ','");
         end if;

         Skip_Whitespace;

      end loop;

      if Current_Character = '}' then
         Next_Character;
      else
         Error ("missing close brace");
      end if;
   end Parse_Json_Object;

   --------------------------
   -- Parse_Rest_Of_String --
   --------------------------

   function Parse_Rest_Of_String return String is
      Result : String (1 .. 1000);
      Last   : Natural := 0;
   begin
      while not Done and then Current_Character /= '"' loop
         if Current_Character = '\' then
            Next_Character;
         end if;
         Last := Last + 1;
         Result (Last) := Current_Character;
         Next_Character;
         if Last = Result'Last then
            return Result & Parse_Rest_Of_String;
         end if;
      end loop;

      if Done then
         Error ("missing close quote");
      end if;

      Next_Character;
      return Result (1 .. Last);
   end Parse_Rest_Of_String;

   --------------------
   -- Parse_Terminal --
   --------------------

   function Parse_Terminal return String is
      Result : String (1 .. 100);
      Last   : Natural := 0;
   begin
      if Current_Character = '"' then
         Next_Character;
         return Parse_Rest_Of_String;
      elsif Current_Character in '0' .. '9' | '+' | '-' then
         if Current_Character in '-' | '+' then
            Last := Last + 1;
            Result (Last) := Current_Character;
            Next_Character;
         end if;

         while not Done
           and then Current_Character in
             '0' .. '9' | '.' | 'e' | 'E' | '+' | '-'
         loop
            Last := Last + 1;
            Result (Last) := Current_Character;
            Next_Character;
         end loop;
         return Result (1 .. Last);
      elsif Ada.Characters.Handling.Is_Letter (Current_Character) then
         while not Done
           and then Ada.Characters.Handling.Is_Alphanumeric
             (Current_Character)
         loop
            Last := Last + 1;
            Result (Last) := Current_Character;
            Next_Character;
         end loop;
         return Result (1 .. Last);
      else
         Error ("expected a name");
         return "";
      end if;
   end Parse_Terminal;

   ---------------------
   -- Skip_Whitespace --
   ---------------------

   procedure Skip_Whitespace is
   begin
      while not Done
        and then (Ada.Characters.Handling.Is_Space (Current_Character)
                  or else Current_Character = Character'Val (9))
      loop
         Next_Character;
      end loop;
   end Skip_Whitespace;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Current_Last := 0;
      Current_Line_Index := 0;
      Current_Index := 1;
      Done := False;
      Next_Character;
   end Start;

end Tropos.Reader.Json;
