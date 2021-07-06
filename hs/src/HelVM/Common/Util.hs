module HelVM.Common.Util (
  toUppers,
  splitOneOf,
  showP,
  showToText,
  genericChr,
  (???),
  fromMaybeOrDef,
) where

import Data.Char hiding (chr)
import Data.Default
import Data.Typeable
import Text.Pretty.Simple

import qualified Data.Text as Text

--- TextUtil

toUppers :: Text -> Text
toUppers = Text.map toUpper

splitOneOf :: String -> Text -> [Text]
splitOneOf s = Text.split contains where contains c = c `elem` s

showP :: Show a => a -> Text
showP = toText . pShowNoColor

showToText :: (Typeable a , Show a) => a -> Text
showToText a = show a `fromMaybe` (cast a :: Maybe Text)

---- CharUtil

genericChr :: Integral a => a -> Char
genericChr = chr . fromIntegral

---- MaybeUtil

infixl 1 ???
(???) :: Maybe a -> a -> a
(???) = flip fromMaybe

fromMaybeOrDef :: Default a => Maybe a -> a
fromMaybeOrDef = fromMaybe def
