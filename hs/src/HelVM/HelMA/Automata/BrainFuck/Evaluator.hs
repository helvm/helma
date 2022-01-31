module HelVM.HelMA.Automata.BrainFuck.Evaluator where

import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator.FlatEvaluator as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Evaluator.TreeEvaluator as Tree

import           HelVM.HelMA.Automata.BrainFuck.Symbol
import           HelVM.HelMA.Automata.BrainFuck.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType

simpleEval :: BIO m => (Bool , Source , CellType) -> m ()
simpleEval (c , s , t) = eval c s t Pretty

----

evalParams :: BIO m => EvalParams -> m ()
evalParams p = eval (compile p) (source p) (cellTypeOptions p) (dumpTypeOptions p)

eval :: BIO m => Bool -> Source -> CellType -> DumpType -> m ()
eval c s Int8Type   = evalSource c s (newTape :: FullTape Int8)
eval c s Word8Type  = evalSource c s (newTape :: FullTape Word8)
eval c s Int16Type  = evalSource c s (newTape :: FullTape Int16)
eval c s Word16Type = evalSource c s (newTape :: FullTape Word16)
eval c s Int32Type  = evalSource c s (newTape :: FullTape Int32)
eval c s Word32Type = evalSource c s (newTape :: FullTape Word32)
eval c s Int64Type  = evalSource c s (newTape :: FullTape Int64)
eval c s Word64Type = evalSource c s (newTape :: FullTape Word64)

evalSource :: (BIO m , Symbol e) => Bool -> Source -> FullTape e -> DumpType -> m ()
evalSource False = Flat.evalSource
evalSource True  = Tree.evalSource
