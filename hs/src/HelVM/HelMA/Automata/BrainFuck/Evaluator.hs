module HelVM.HelMA.Automata.BrainFuck.Evaluator where

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Evaluator  as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator  as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator  as Tree

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Loop

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType

simpleEval :: BIO m => (BFType , Source , CellType) -> m ()
simpleEval (c , s , t) = eval c s t Pretty --TODO Add MaybeLimit and use Loop

----

evalParams :: BIO m => BFType -> EvalParams -> m ()
evalParams b p = eval b (source p) (cellAutoOptions p) Nothing (dumpAutoOptions p)

eval :: BIO m => BFType -> Source -> CellType -> DumpType -> m ()
eval c s Int8Type   = evalSource c s (newTape :: FullTape Int8)
eval c s Word8Type  = evalSource c s (newTape :: FullTape Word8)
eval c s Int16Type  = evalSource c s (newTape :: FullTape Int16)
eval c s Word16Type = evalSource c s (newTape :: FullTape Word16)
eval c s Int32Type  = evalSource c s (newTape :: FullTape Int32)
eval c s Word32Type = evalSource c s (newTape :: FullTape Word32)
eval c s Int64Type  = evalSource c s (newTape :: FullTape Int64)
eval c s Word64Type = evalSource c s (newTape :: FullTape Word64)

evalSource :: (BIO m , Symbol e) => BFType -> Source -> FullTape e -> DumpType -> m ()
evalSource FastType = Fast.evalSource
evalSource TreeType = Tree.evalSource
evalSource FlatType = Flat.evalSource
