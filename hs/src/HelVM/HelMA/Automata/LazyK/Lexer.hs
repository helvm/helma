module HelVM.HelMA.Automata.LazyK.Lexer where

import           HelVM.HelMA.Automaton.API.IOTypes

import qualified Data.Text                         as Text

filterComments :: Source -> Source
filterComments source = mconcat $ removeComment <$> lines source

removeComment :: Source -> Source
removeComment = fst . Text.break isHash

isHash :: Char -> Bool
isHash c = '#' == c
