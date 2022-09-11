module HelVM.HelMA.Automata.SubLeq.Automaton (
  simpleRun,
  runWithParams,
  run,
) where

import           HelVM.HelMA.Automata.SubLeq.Evaluator
import           HelVM.HelMA.Automata.SubLeq.Lexer

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType

import qualified HelVM.HelIO.Collections.MapList       as MapList
import qualified HelVM.HelIO.Collections.SList         as SList

import qualified Data.Sequence                         as Seq

simpleRun :: BIO m => RAMType -> Source -> m ()
simpleRun rt s = run s rt Pretty

----

runWithParams :: BIO m => RunParams -> m ()
runWithParams p = run (source p) (ramTypeOptions p) (dumpTypeOptions p)

run :: BIO m => Source -> RAMType -> DumpType -> m ()
run source = evalIL $ tokenize source

evalIL :: Evaluator e m => [e] -> RAMType -> DumpType -> m ()
evalIL = flip evalIL'

evalIL' :: Evaluator e m => RAMType -> [e] -> DumpType -> m ()
evalIL' ListRAMType    = start
evalIL' SeqRAMType     = start . Seq.fromList
evalIL' SListRAMType   = start . SList.sListFromList
evalIL' MapListRAMType = start . MapList.mapListFromList

start :: REvaluator e r m => r -> DumpType -> m ()
start r dt = logDump dt =<< doInstruction 0 r
