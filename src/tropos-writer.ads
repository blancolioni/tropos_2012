package Tropos.Writer is

   procedure Write_Config (Config : Configuration;
                           Path   : String);

   procedure Write_XML_Config (Config : Configuration;
                               Path   : String);

   procedure Write_Scheme_Config (Config : Configuration;
                                  Path   : String);

end Tropos.Writer;
