with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Tropos.Properties_Reader is

   function Trim_Line (Line : String) return String;

   ---------------------
   -- Read_Properties --
   ---------------------

   function Read_Properties (Path : String) return Configuration is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      Result : Configuration :=
                 New_Config (Ada.Directories.Base_Name
                             (Ada.Directories.Simple_Name (Path)));
      File   : File_Type;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Full_Line : constant String := Get_Line (File);
            Line      : constant String := Trim_Line (Full_Line);
            Equal     : constant Natural := Index (Line, "=");
         begin
            if Line'Length = 0
              or else Line (Line'First) = '#'
              or else Line (Line'First) = Character'Val (13)
            then
               null;
            elsif Equal > 0 then
               Result.Set_Path (Line (Line'First .. Equal - 1),
                                Line (Equal + 1 .. Line'Last),
                               '.');
            else
               Put_Line (Standard_Error,
                         "bad line: " & Line);
            end if;
         end;
      end loop;
      Close (File);

      return Result;

   end Read_Properties;

   ---------------
   -- Trim_Line --
   ---------------

   function Trim_Line (Line : String) return String is
      Result : constant String :=
                 Ada.Strings.Fixed.Trim
                   (Source =>
                      (if Character'Pos (Line (Line'Last)) = 13
                       then Line (Line'First .. Line'Last - 1)
                       else Line),
                    Side   => Ada.Strings.Both);
   begin
      return Result;
   end Trim_Line;

end Tropos.Properties_Reader;
