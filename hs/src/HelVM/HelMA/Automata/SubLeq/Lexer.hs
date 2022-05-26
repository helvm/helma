module HelVM.HelMA.Automata.SubLeq.Lexer where

import           HelVM.HelMA.Automata.SubLeq.Symbol

import           HelVM.HelIO.ReadText
import           HelVM.HelIO.Util
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified Text.Read                          as Read
import qualified Text.Show                          as Show

tokenize :: Source -> SymbolList
tokenize source = (maybeToList . readTextMaybe) =<< splitOneOf " \t\n" source

readSymbols :: Source -> Symbols
readSymbols source = readTextUnsafe source :: Symbols

----

newtype Symbols = Symbols SymbolList

instance Show Symbols where
  show (Symbols symbols) = toString $ unwords $ shows symbols

instance Read Symbols where
  readsPrec _ source = [( Symbols $ tokenize $ toText source , "")]

----

shows :: SymbolList -> [Text]
shows symbols = show <$> symbols
