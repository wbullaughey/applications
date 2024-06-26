package Video.Configuration is

   type Configuration_Type is tagged null record;

   procedure Load (
      Configuration              :  out Configuration_Type);

end Video.Configuration;

