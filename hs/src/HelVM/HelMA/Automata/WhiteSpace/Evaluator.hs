module HelVM.HelMA.Automata.WhiteSpace.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams as S
import           HelVM.HelMA.Automata.WhiteSpace.Token

import qualified HelVM.HelMA.Automaton.API.AutomatonOptions   as Automaton
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Automaton

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.FormatType
import           HelVM.HelMA.Automaton.Types.TokenType

import           HelVM.HelIO.Control.Safe

import           Prelude                                      hiding (swap)


simpleEval :: BIO m => S.SimpleParams -> m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.formatType p) $ S.automatonOptions p

----

evalParams :: BIO m => TokenType -> EvalParams -> m ()
evalParams tokenType p = eval tokenType (source p) (formatType p) $ automatonOptions p

eval :: BIO m => TokenType -> Source -> FormatType -> Automaton.AutomatonOptions -> m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL :: BIO m => TokenList -> FormatType -> Automaton.AutomatonOptions -> m ()
evalTL tl ascii ao = flip start ao =<< liftSafe (parseFromTL ascii tl)
