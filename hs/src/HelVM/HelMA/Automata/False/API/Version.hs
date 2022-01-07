module HelVM.HelMA.Automata.False.API.Version where

data Version = ThulsaDum | ThunderSeethe
  deriving stock (Eq , Read , Show)

versions :: [Version]
versions = [ThulsaDum , ThunderSeethe]

defaultVersion :: Version
defaultVersion = ThulsaDum

parseVersion :: String -> Version
parseVersion raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid Version. Valid version are : " <> show versions
