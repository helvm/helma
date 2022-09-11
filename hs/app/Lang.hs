module Lang where

parseLang :: String -> Lang
parseLang raw = (valid . readMaybe) raw where
  valid (Just a) = a
  valid Nothing  = error $ "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs

langs :: [Lang]
langs = [Cat , Rev , BF , ETA , Lazy , SQ , WS , Zot]

data Lang = Cat | Rev | BF | ETA | Lazy | SQ | WS | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
