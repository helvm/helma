module HelVM.HelMA.Automata.SubLeq.Lexer where

import HelVM.HelMA.Automata.SubLeq.Symbol
import HelVM.HelMA.Common.Util

import Data.List.Split

import qualified Data.String as String
import qualified Text.Read as Read
import qualified Text.Show

tokenize :: Source -> SymbolList
tokenize source = (maybeToList . readMaybe) =<< splitOneOf " \t\n" source

readSymbols :: String -> Symbols
readSymbols source = Read.read source :: Symbols

----

newtype Symbols = Symbols SymbolList

instance Show Symbols where
  show (Symbols symbols) = String.unwords $ show <$> symbols

instance Read Symbols where
  readsPrec _ source = [( Symbols $ (maybeToList . readMaybe) =<< splitOneOf " \t\n" source , "")]
