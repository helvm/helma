module Lang where

import           HelVM.HelIO.Extra
import           HelVM.HelIO.SwitchEnum

parseLang :: String -> Lang
parseLang raw = fromJustWithText message $ readMaybe raw where
  message = "Lang '" <> toText raw <> "' is not valid lang. Valid langs are : " <> show langs

langs :: [Lang]
langs = generateEnums 9

defaultLang :: Lang
defaultLang = defaultEnum

data Lang = Cat | Rev | BF | ETA | F | Lazy | SQ | WS | Zot
  deriving stock (Bounded , Enum , Eq , Read , Show)
