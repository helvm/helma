module HelVM.HelMA.Automata.WhiteSpace.Evaluator
  ( emitCode
  , emitIL
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
import           HelVM.HelMA.Automata.WhiteSpace.Token

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions    as Automaton
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Automaton
import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.LabelType

import           HelVM.HelIO.Control.Safe

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
emitIL t p = printIL <$> parse2 (t, labelType p) (source p)

emitTL ∷ TokenType → Source → LText
emitTL t = show . tokenize t

emitCode ∷ TokenType → Source → LText
emitCode VisibleTokenType = show . readVisibleTokens
emitCode WhiteTokenType   = show . readWhiteTokens

simpleEval ∷ AppSafeEff m ⇒ S.SimpleParams → m ()
simpleEval p = eval (S.tokenType p) (S.source p) (S.labelType p) $ S.automatonOptions p

----

evalParams ∷ AppSafeEff m ⇒ TokenType → EvalParams → m ()
evalParams tokenType p = eval tokenType (source p) (labelType p) $ automatonOptions p

eval ∷ AppSafeEff m ⇒ TokenType → Source → LabelType → Automaton.AutomatonOptions → m ()
eval tokenType source = evalTL $ tokenize tokenType source

evalTL ∷ AppSafeEff m ⇒ TokenList → LabelType → Automaton.AutomatonOptions → m ()
evalTL tl ascii ao = flip start ao =<< liftSafe (parseFromTL ascii tl)
