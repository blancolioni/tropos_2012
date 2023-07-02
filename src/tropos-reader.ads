package Tropos.Reader is

   function Read_Config (Path : String) return Configuration;
   function Read_Json_Config (Path : String) return Configuration;

   function Read_Config (Path      : String;
                         Extension : String)
                         return Configuration;

   procedure Read_Config (Path : String;
                          Extension : String;
                          Configure : not null access procedure
                          (Config : Configuration));

   procedure Read_Config (Path : String;
                          Configure : not null access procedure
                          (Config : Configuration));

   function Read_Indirect_Config (Path : String)
                                  return Configuration;

   function Read_CSV_Config
     (Path          : String;
      Header_Line   : Boolean := True;
      Separator     : Character := ',';
      Extend_Header : Boolean := True)
      return Configuration;

   Escape_Character : Character := '\';

end Tropos.Reader;
