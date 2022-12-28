module HelVM.HelMA.Automata.SubLeq.Evaluator (
  simpleEval,
  evalParams,
) where

import           HelVM.HelMA.Automata.SubLeq.Automaton
import           HelVM.HelMA.Automata.SubLeq.Lexer

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Loop

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.HelIO.Collections.MapList       as MapList
import qualified HelVM.HelIO.Collections.SList         as SList

import qualified Data.Sequence                         as Seq

simpleEval :: BIO m => RAMType -> Source -> m ()
simpleEval rt s = evalSource s rt testMaybeLimit Pretty

----

evalParams :: BIO m => EvalParams -> m ()
evalParams p = evalSource (source p) (ramAutoOptions p) Nothing (dumpAutoOptions p)

evalSource :: BIO m => Source -> RAMType -> LimitMaybe -> DumpType -> m ()
evalSource source = evalIL $ tokenize source

evalIL :: AutomatonIO e m => [e] -> RAMType -> LimitMaybe -> DumpType -> m ()
evalIL = flip evalIL'

evalIL' :: AutomatonIO e m => RAMType -> [e] -> LimitMaybe -> DumpType -> m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . MapList.mapListFromList

start :: RAutomatonIO e r m => r -> LimitMaybe -> DumpType -> m ()
start r limit dt = logDump dt =<< run limit (newAutomaton r)
