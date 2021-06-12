module HelVM.Common.Util where

import Data.Char hiding (chr)
import Data.Default
import Data.Typeable

import qualified Data.Text as Text

type D a = a -> a

--- TextUtil

toUppers :: Text -> Text
toUppers = Text.map toUpper

splitOneOf :: String -> Text -> [Text]
splitOneOf s = Text.split contains where contains c = c `elem` s

showToText :: (Typeable a, Show a) => a -> Text
showToText a = show a `fromMaybe` (cast a :: Maybe Text)

---- CharUtil

genericChr :: Integral a => a -> Char
genericChr = chr . fromIntegral

---- MaybeUtil

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

fromMaybeOrDef :: Default a => Maybe a -> a
fromMaybeOrDef = fromMaybe def
