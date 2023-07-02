package body Tropos.Generic_Config is

   ---------
   -- Get --
   ---------

   function Get
     (Field_Name : String)
      return Integer
   is
   begin
      return Config.Get (Field_Name);
   end Get;

end Tropos.Generic_Config;
