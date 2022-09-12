module Lang where

import           HelVM.HelIO.Extra

parseLang :: String -> Lang
parseLang raw = fromJustWithText message $ readMaybe raw where
  message = "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs

langs :: [Lang]
langs = [Cat , Rev , BF , ETA , Lazy , SQ , WS , Zot]

data Lang = Cat | Rev | BF | ETA | Lazy | SQ | WS | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
