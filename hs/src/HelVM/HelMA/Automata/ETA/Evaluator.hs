module HelVM.HelMA.Automata.ETA.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer
import qualified HelVM.HelMA.Automata.ETA.SimpleParams      as S
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.AutoOptions
import qualified HelVM.HelMA.Automaton.API.AutomatonOptions as Automaton
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified HelVM.HelMA.Automaton.Automaton            as Automaton

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList              as SList

import qualified Data.Sequence                              as Seq

import           Prelude                                    hiding (divMod)

simpleEval :: BIO m => S.SimpleParams -> m ()
simpleEval p = evalSource (S.implType p) (S.source p) (S.stackType p) (S.autoOptions p)

----

evalParams :: BIO m => ETAImplType -> EvalParams -> m ()
evalParams e p = evalSource e (source p) (stackAutoOptions p) (autoOptions p)

evalSource :: (AutomatonIO Symbol m) => ETAImplType -> Source -> StackType -> AutoOptions -> m ()
evalSource etaImplType source = evalTL etaImplType (tokenize source)

evalTL :: (AutomatonIO Symbol m) => ETAImplType -> TokenList -> StackType -> AutoOptions -> m ()
evalTL Fast     = fastEval
evalTL Original = originalEval

fastEval :: (AutomatonIO Symbol m) => TokenList -> StackType -> AutoOptions -> m ()
fastEval tl s a = flip Automaton.start (Automaton.withDefaultRam s a) =<< optimize tl

originalEval :: (AutomatonIO Symbol m) => TokenList -> StackType -> AutoOptions -> m ()
originalEval tl ListStackType  = eval tl []
originalEval tl SeqStackType   = eval tl Seq.empty
originalEval tl SListStackType = eval tl SList.sListEmpty

eval :: (SAutomatonIO Symbol s m) => TokenList -> s -> AutoOptions -> m ()
eval tl s (AutoOptions _ limit dt) = logDump dt =<< run limit (newMemory tl s)
