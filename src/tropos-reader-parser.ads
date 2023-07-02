package Tropos.Reader.Parser is

   Parse_Error : exception;

   type Token is (Tok_Name,
                  Tok_Equal,
                  Tok_Open_Brace,
                  Tok_Close_Brace,
                  Tok_Open_Paren,
                  Tok_Close_Paren);

   procedure Open (Path : String);
   procedure Close;

   function Tok return Token;
   function Tok_Text return String;

   procedure Next;

   function End_Of_File return Boolean;

   procedure Error (Message : String);

end Tropos.Reader.Parser;
