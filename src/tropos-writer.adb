with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Tropos.Writer is

   ------------------
   -- Write_Config --
   ------------------

   procedure Write_Config
     (Config : Configuration;
      Path   : String)
   is
      procedure Write_Config (Item   : Configuration;
                              Indent : Ada.Text_IO.Positive_Count);

      function Maybe_Quote (Text : String) return String;
      function Escape_Special (Text : String) return String;

      -------------------
      -- Escape_Quotes --
      -------------------

      function Escape_Special (Text : String) return String is
         use Ada.Characters.Latin_1;
         Result   : String (1 .. Text'Length * 2);
         Length   : Natural := 0;
         Got_CRLF : Boolean := False;
      begin
         for I in Text'Range loop
            if Text (I) = '"' then
               Length := Length + 1;
               Result (Length) := '\';
               Length := Length + 1;
               Result (Length) := '"';
            elsif Text (I) = CR then
               if I < Text'Last and then
                 Text (I + 1) = LF
               then
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
                  Got_CRLF := True;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'r';
               end if;
            elsif Text (I) = LF then
               if Got_CRLF then
                  Got_CRLF := False;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
               end if;
            elsif Text (I) = HT then
               Length := Length + 1;
               Result (Length) := ' ';
            else
               Length := Length + 1;
               Result (Length) := Text (I);
            end if;
         end loop;
         return Result (1 .. Length);
      end Escape_Special;

      -----------------
      -- Maybe_Quote --
      -----------------

      function Maybe_Quote (Text : String) return String is
         use Ada.Characters.Handling;
      begin
         if Text'Length = 0 then
            return '"' & '"';
         end if;
         for I in Text'Range loop
            if not Is_Letter (Text (I))
              and then not Is_Digit (Text (I))
              and then Text (I) not in '_' | '.' | '-' | '+'
            then
               return '"' & Escape_Special (Text) & '"';
            end if;
         end loop;
         return Text;
      end Maybe_Quote;

      ------------------
      -- Write_Config --
      ------------------

      procedure Write_Config (Item   : Configuration;
                              Indent : Ada.Text_IO.Positive_Count)
      is
         use Ada.Text_IO;
      begin
         Set_Col (Indent);
         Put (Maybe_Quote (Item.Config_Name));
         if Child_Count (Item) = 0 then
            New_Line;
         elsif Child_Count (Item) = 1 and then
           Item.Children.Element (1).Child_Count = 0
         then
            Put (" = ");
            Put (Maybe_Quote (Item.Children.Element (1).Config_Name));
            New_Line;
         else
            Put (" = {");
            declare
               New_Indent : constant Positive_Count :=
                 Col + 1;
            begin
               for I in 1 .. Child_Count (Item) loop
                  Write_Config (Item.Children.Element (I).all, New_Indent);
               end loop;
               Set_Col (New_Indent - 2);
               Put_Line ("}");
            end;
         end if;

      end Write_Config;

      File : Ada.Text_IO.File_Type;
   begin
      if Path /= "" then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
         Ada.Text_IO.Set_Output (File);
      end if;

      for I in 1 .. Child_Count (Config) loop
         Write_Config (Config.Children.Element (I).all, 1);
      end loop;

      if Path /= "" then
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (File);
      end if;

   end Write_Config;

   -------------------------
   -- Write_Scheme_Config --
   -------------------------

   procedure Write_Scheme_Config
     (Config : Configuration;
      Path   : String)
   is
      procedure Write_S_Expression
        (Item   : Configuration;
         Indent : Ada.Text_IO.Positive_Count);

      procedure Write_Simple_S_Expression
        (Item   : Configuration);

      function Maybe_Quote (Text : String) return String;
      function Escape_Special (Text : String) return String;

      -------------------
      -- Escape_Quotes --
      -------------------

      function Escape_Special (Text : String) return String is
         use Ada.Characters.Latin_1;
         Result   : String (1 .. Text'Length * 2);
         Length   : Natural := 0;
         Got_CRLF : Boolean := False;
      begin
         for I in Text'Range loop
            if Text (I) = '"' then
               Length := Length + 1;
               Result (Length) := '\';
               Length := Length + 1;
               Result (Length) := '"';
            elsif Text (I) = CR then
               if I < Text'Last and then
                 Text (I + 1) = LF
               then
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
                  Got_CRLF := True;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'r';
               end if;
            elsif Text (I) = LF then
               if Got_CRLF then
                  Got_CRLF := False;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
               end if;
            elsif Text (I) = HT then
               Length := Length + 1;
               Result (Length) := ' ';
            else
               Length := Length + 1;
               Result (Length) := Text (I);
            end if;
         end loop;
         return Result (1 .. Length);
      end Escape_Special;

      -----------------
      -- Maybe_Quote --
      -----------------

      function Maybe_Quote (Text : String) return String is
         use Ada.Characters.Handling;
         use Ada.Strings.Fixed;
      begin
         if Text'Length = 0 then
            return '"' & '"';
         end if;
         for I in Text'Range loop
            if not Is_Letter (Text (I)) and then
              not Is_Digit (Text (I)) and then
              Index ("!$%&*+-./:<=>?@^_~#",
                     (1 => Text (I))) = 0
            then
               return '"' & Escape_Special (Text) & '"';
            end if;
         end loop;
         return Text;
      end Maybe_Quote;

      ------------------------
      -- Write_S_Expression --
      ------------------------

      procedure Write_S_Expression
        (Item   : Configuration;
         Indent : Ada.Text_IO.Positive_Count)
      is
         use Ada.Text_IO;
      begin
         Set_Col (Indent);
         Put ("(" & Item.Config_Name);

         for Child of Item loop
            Write_Simple_S_Expression (Child);
         end loop;

         for Child of Item loop
            if Child.Child_Count > 1
              or else (Child.Child_Count = 1
                       and then Child.Children.Element (1).Child_Count > 0)
            then
               New_Line;
               Write_S_Expression (Child, Indent + 2);
            end if;
         end loop;

         if Col > 72 then
            Set_Col (Indent);
         end if;

         Put (")");

      end Write_S_Expression;

      -------------------------------
      -- Write_Simple_S_Expression --
      -------------------------------

      procedure Write_Simple_S_Expression
        (Item   : Configuration)
      is
         use Ada.Text_IO;
      begin
         if Child_Count (Item) = 0 then
            Put (" " & Item.Config_Name);
         elsif Child_Count (Item) = 1 and then
           Item.Children.Element (1).Child_Count = 0
         then
            Put (" (" & Item.Config_Name & " ");
            declare
               Value : constant String :=
                         Item.Children.Element (1).Config_Name;
            begin
               if Value = "yes" or else Value = "true" then
                  Put ("#t");
               elsif Value = "no" or else Value = "false" then
                  Put ("#f");
               else
                  Put (Maybe_Quote (Item.Children.Element (1).Config_Name));
               end if;
               Put (")");
            end;

         end if;
      end Write_Simple_S_Expression;

      File : Ada.Text_IO.File_Type;
   begin
      if Path /= "" then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
         Ada.Text_IO.Set_Output (File);
      end if;

      Ada.Text_IO.Put_Line
        ("; Scheme config generated by Tropos");

      Write_S_Expression (Config, 1);

      if Path /= "" then
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (File);
      end if;

   end Write_Scheme_Config;

   ----------------------
   -- Write_XML_Config --
   ----------------------

   procedure Write_XML_Config
     (Config : Configuration;
      Path   : String)
   is
      procedure Write_XML_Tag
        (Item   : Configuration;
         Indent : Ada.Text_IO.Positive_Count);

      procedure Write_XML_Attribute
        (Item   : Configuration);

      function Maybe_Quote (Text : String) return String;
      function Escape_Special (Text : String) return String;

      -------------------
      -- Escape_Quotes --
      -------------------

      function Escape_Special (Text : String) return String is
         use Ada.Characters.Latin_1;
         Result   : String (1 .. Text'Length * 2);
         Length   : Natural := 0;
         Got_CRLF : Boolean := False;
      begin
         for I in Text'Range loop
            if Text (I) = '"' then
               Length := Length + 1;
               Result (Length) := '\';
               Length := Length + 1;
               Result (Length) := '"';
            elsif Text (I) = CR then
               if I < Text'Last and then
                 Text (I + 1) = LF
               then
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
                  Got_CRLF := True;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'r';
               end if;
            elsif Text (I) = LF then
               if Got_CRLF then
                  Got_CRLF := False;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
               end if;
            elsif Text (I) = HT then
               Length := Length + 1;
               Result (Length) := ' ';
            else
               Length := Length + 1;
               Result (Length) := Text (I);
            end if;
         end loop;
         return Result (1 .. Length);
      end Escape_Special;

      -----------------
      -- Maybe_Quote --
      -----------------

      function Maybe_Quote (Text : String) return String is
         use Ada.Characters.Handling;
      begin
         if Text'Length = 0 then
            return '"' & '"';
         end if;
         for I in Text'Range loop
            if not Is_Letter (Text (I)) and then
              not Is_Digit (Text (I)) and then
              Text (I) /= '_' and then
              Text (I) /= '.'
            then
               return '"' & Escape_Special (Text) & '"';
            end if;
         end loop;
         return Text;
      end Maybe_Quote;

      -------------------------
      -- Write_XML_Attribute --
      -------------------------

      procedure Write_XML_Attribute
        (Item   : Configuration)
      is
         use Ada.Text_IO;
      begin
         if Child_Count (Item) = 0 then
            Put (" " & Item.Config_Name & "=""true""");
         elsif Child_Count (Item) = 1 and then
           Item.Children.Element (1).Child_Count = 0
         then
            Put (" " & Item.Config_Name & "=""");
            Put (Maybe_Quote (Item.Children.Element (1).Config_Name));
            Put ("""");
         end if;
      end Write_XML_Attribute;

      -------------------
      -- Write_XML_Tag --
      -------------------

      procedure Write_XML_Tag
        (Item   : Configuration;
         Indent : Ada.Text_IO.Positive_Count)
      is
         use Ada.Text_IO;
      begin
         Set_Col (Indent);
         Put ("<" & Item.Config_Name);

         for Child of Item loop
            Write_XML_Attribute (Child);
         end loop;

         Put_Line (">");
         for Child of Item loop
            if Child.Child_Count > 1
              or else (Child.Child_Count = 1
                       and then Child.Children.Element (1).Child_Count > 0)
            then
               Write_XML_Tag (Child, Indent + 2);
            end if;
         end loop;
         Set_Col (Indent);
         Put_Line ("</" & Item.Config_Name & ">");

      end Write_XML_Tag;

      File : Ada.Text_IO.File_Type;
   begin
      if Path /= "" then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
         Ada.Text_IO.Set_Output (File);
      end if;

      Ada.Text_IO.Put_Line
        ("<?xml version=""1.0"" encoding=""UTF-8""?>");

      Write_XML_Tag (Config, 1);

      if Path /= "" then
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (File);
      end if;

   end Write_XML_Config;

end Tropos.Writer;
