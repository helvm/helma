module HelVM.HelMA.Automata.WhiteSpace.Evaluator
  ( emitCode
  , emitIL
  , emitILForTest
  , emitTL
  , evalParams
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.WhiteSpace.API.TokenType
import           HelVM.HelMA.Automata.WhiteSpace.Lexer
import           HelVM.HelMA.Automata.WhiteSpace.Parser
import qualified HelVM.HelMA.Automata.WhiteSpace.SimpleParams  as S

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions    as Automaton
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.ParserOptions

import           HelVM.HelMA.Automaton.Automaton
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import           Prelude                                       hiding ( swap )

import qualified RIO

runRio ∷ Has env ⇒ TokenType → RIO.RIO env ()
runRio t = runWithOptions =<< optionsRio where
  runWithOptions o = run (App.emit o) t . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit → TokenType → EvalParams → RIO.RIO env ()
run No   t = runAsRIO . evalParams t
run IL   t = putLTextLnRio <=< runAsRIO . emitIL t
run TL   t = putLTextLnRio . emitTL t . source
run Code t = putLTextLnRio . emitCode t . source

emitIL ∷ MonadSafe m ⇒ TokenType → EvalParams → m LText
emitIL t p = emitILForTest (parserOptions p) t (source p)

emitILForTest ∷ MonadSafe m ⇒ ParserOptions → TokenType → Source → m LText
emitILForTest parserOptions tokenType = printIL <.> optimize (optLevel parserOptions) <.> parseIL parserOptions tokenType

emitTL ∷ TokenType → Source → LText
emitTL t = show . tokenize t

emitCode ∷ TokenType → Source → LText
emitCode VisibleTokenType = show . readVisibleTokens
emitCode WhiteTokenType   = show . readWhiteTokens

simpleEval ∷ AppSafeEff m ⇒ S.SimpleParams → m ()
simpleEval p = eval (S.automatonOptions p) (simpleAutoParams (S.labelType p)) (S.tokenType p) (S.source p)

----

evalParams ∷ AppSafeEff m ⇒ TokenType → EvalParams → m ()
evalParams tokenType p = eval (automatonOptions p) (parserOptions p) tokenType (source p)

eval ∷ AppSafeEff m ⇒ Automaton.AutomatonOptions → ParserOptions → TokenType → Source →m ()
eval ao parserOptions tokenType source = evalIL ao =<< parseIL  parserOptions tokenType source

evalIL ∷ AppSafeEff m ⇒ Automaton.AutomatonOptions → InstructionList → m ()
evalIL = flip start
