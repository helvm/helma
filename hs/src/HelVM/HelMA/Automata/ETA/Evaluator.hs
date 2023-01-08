module HelVM.HelMA.Automata.ETA.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.ETA.Automaton
import           HelVM.HelMA.Automata.ETA.Lexer
import           HelVM.HelMA.Automata.ETA.Optimizer
import           HelVM.HelMA.Automata.ETA.Symbol
import           HelVM.HelMA.Automata.ETA.Token

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import qualified HelVM.HelMA.Automaton.Automaton          as Automaton

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Loop

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelIO.Collections.SList            as SList

import           Prelude                                  hiding (divMod)

import qualified Data.Sequence                            as Seq

import           HelVM.HelMA.Automata.ETA.API.ETAImplType

simpleEval :: BIO m => (ETAImplType , Source , StackType) -> m ()
simpleEval (c , s , t) = evalSource c s t testLoopLimit Pretty

----

evalParams :: BIO m => ETAImplType -> EvalParams -> m ()
evalParams e p = evalSource e (source p) (stackAutoOptions p) Nothing (dumpAutoOptions p)

evalSource :: (AutomatonIO Symbol m) => ETAImplType -> Source -> StackType -> LoopLimit -> DumpType -> m ()
evalSource etaImplType source = evalTL etaImplType (tokenize source)

evalTL :: (AutomatonIO Symbol m) => ETAImplType -> TokenList -> StackType -> LoopLimit -> DumpType -> m ()
evalTL c tl ListStackType  = eval c tl []
evalTL c tl SeqStackType   = eval c tl Seq.empty
evalTL c tl SListStackType = eval c tl SList.sListEmpty

eval :: (SAutomatonIO Symbol s m) => ETAImplType -> TokenList -> s -> LoopLimit -> DumpType -> m ()
eval Original tl s limit dt = logDump dt =<< run limit (newAutomaton tl s)
eval Fast     tl s limit dt = logDump dt =<< (Automaton.run limit . Automaton.flippedNewAutomaton (s , [])) =<< optimize tl
