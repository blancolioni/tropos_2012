with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Tropos.Reader.Parser is

   File : Ada.Text_IO.File_Type;

   Curr_File_Name   : Ada.Strings.Unbounded.Unbounded_String;
   Curr_Token       : Token;
   Curr_Line        : String (1 .. 4096);
   Curr_Line_Length : Natural;
   Curr_Line_Number : Natural;
   Curr_Col_Index   : Natural;
   Curr_Token_Text  : String (1 .. 1024);
   Curr_Text_Length : Natural;

   End_Of_Line    : Boolean;
   Is_End_Of_File : Boolean;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin
      Ada.Text_IO.Close (File);
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File return Boolean is
   begin
      return Is_End_Of_File;
   end End_Of_File;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            Ada.Strings.Unbounded.To_String (Curr_File_Name) &
                            ", line" & Curr_Line_Number'Img &
                            ", column" & Curr_Col_Index'Img &
                            ": " & Message &
                            " at " & Curr_Token'Img);
      raise Parse_Error;
   end Error;

   ----------
   -- Next --
   ----------

   procedure Next is
   begin
      while not Ada.Text_IO.End_Of_File (File) or else not End_Of_Line loop
         if End_Of_Line then
            Curr_Line_Number := Curr_Line_Number + 1;
            Ada.Text_IO.Get_Line (File, Curr_Line, Curr_Line_Length);
            if Curr_Line_Length > 0
              and then Curr_Line (Curr_Line_Length) = Character'Val (13)
            then
               Curr_Line_Length := Curr_Line_Length - 1;
            end if;

            Curr_Col_Index := 1;
            End_Of_Line := False;
         end if;

         while Curr_Col_Index <= Curr_Line_Length loop
            declare
               Ch : Character := Curr_Line (Curr_Col_Index);
            begin
               Curr_Col_Index := Curr_Col_Index + 1;
               if Ch = '{' then
                  Curr_Token := Tok_Open_Brace;
                  return;
               elsif Ch = '}' then
                  Curr_Token := Tok_Close_Brace;
                  return;
               elsif Ch = '(' then
                  Curr_Token := Tok_Open_Paren;
                  return;
               elsif Ch = ')' then
                  Curr_Token := Tok_Close_Paren;
                  return;
               elsif Ch = '=' then
                  Curr_Token := Tok_Equal;
                  return;
               elsif Ch = '#' then
                  Curr_Col_Index := Curr_Line_Length + 1;
               elsif Ch = '/' and then Curr_Col_Index <= Curr_Line_Length
                 and then Curr_Line (Curr_Col_Index) = '/'
               then
                  Curr_Col_Index := Curr_Line_Length + 1;
               elsif Ch = '"' then
                  Curr_Text_Length := 0;
                  Ch := Curr_Line (Curr_Col_Index);
                  Curr_Col_Index := Curr_Col_Index + 1;
                  while Ch /= '"' and then
                    Curr_Col_Index <= Curr_Line_Length + 1
                  loop
                     declare
                        Escape : constant Boolean :=
                                   Ch = Escape_Character;
                     begin
                        if Escape then
                           Ch := Curr_Line (Curr_Col_Index);
                           Curr_Col_Index := Curr_Col_Index + 1;
                           case Ch is
                              when 'n' =>
                                 Ch := Character'Val (10);
                              when others =>
                                 null;
                           end case;
                        end if;
                     end;

                     Curr_Text_Length := Curr_Text_Length + 1;
                     Curr_Token_Text (Curr_Text_Length) := Ch;
                     Ch := Curr_Line (Curr_Col_Index);
                     Curr_Col_Index := Curr_Col_Index + 1;
                  end loop;
                  Curr_Token := Tok_Name;
                  return;
               elsif Ch = '[' then
                  Curr_Text_Length := 0;

                  loop
                     if Curr_Col_Index > Curr_Line_Length then
                        Curr_Line_Number := Curr_Line_Number + 1;
                        Ada.Text_IO.Get_Line
                          (File, Curr_Line, Curr_Line_Length);
                        if Curr_Line_Length > 0
                          and then Curr_Line (Curr_Line_Length)
                          = Character'Val (13)
                        then
                           Curr_Line_Length := Curr_Line_Length - 1;
                        end if;

                        Curr_Col_Index := 1;
                        End_Of_Line := False;
                        Ch := Character'Val (10);
                     else
                        Ch := Curr_Line (Curr_Col_Index);
                        Curr_Col_Index := Curr_Col_Index + 1;
                        exit when Ch = ']';
                     end if;
                     Curr_Text_Length := Curr_Text_Length + 1;
                     Curr_Token_Text (Curr_Text_Length) := Ch;
                  end loop;
                  Curr_Token := Tok_Name;
--                    Ada.Text_IO.Put_Line
--                      ("[" & Curr_Token_Text (1 .. Curr_Text_Length) & "]");
                  return;
               elsif Ch /= ' ' and then Ch /= Character'Val (9) and then
                 Ch /= ASCII.CR
               then
                  Curr_Text_Length := 0;
                  while Ch /= ' ' and then Ch /= '=' and then
                    Ch /= '{' and then Ch /= '}' and then
                    Ch /= '(' and then Ch /= ')' and then
                    Ch /= ASCII.CR and then
                    Ch /= ASCII.HT and then
                    Ch /= '#' and then Ch /= ';' and then
                    Curr_Col_Index <= Curr_Line_Length + 1
                  loop
                     Curr_Text_Length := Curr_Text_Length + 1;
                     Curr_Token_Text (Curr_Text_Length) := Ch;
                     Ch := Curr_Line (Curr_Col_Index);
                     Curr_Col_Index := Curr_Col_Index + 1;
                  end loop;
                  Curr_Token := Tok_Name;
                  if Ch /= ';' then
                     Curr_Col_Index := Curr_Col_Index - 1;
                  end if;
                  return;
               end if;
            end;
         end loop;

         End_Of_Line := True;
      end loop;

      Is_End_Of_File := True;

   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (Path : String) is
      Local_Path : String := Path;
   begin
      if not Ada.Directories.Exists (Local_Path) then
         Local_Path := Ada.Characters.Handling.To_Lower (Path);
      end if;
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Local_Path);
      End_Of_Line := True;
      Is_End_Of_File := False;
      Curr_Line_Number := 0;
      Curr_File_Name := Ada.Strings.Unbounded.To_Unbounded_String (Path);
      Next;
   end Open;

   ---------
   -- Tok --
   ---------

   function Tok return Token is
   begin
      return Curr_Token;
   end Tok;

   --------------
   -- Tok_Text --
   --------------

   function Tok_Text return String is
   begin
      return Curr_Token_Text (1 .. Curr_Text_Length);
   end Tok_Text;

end Tropos.Reader.Parser;
