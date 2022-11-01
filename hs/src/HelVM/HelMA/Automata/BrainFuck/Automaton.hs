module HelVM.HelMA.Automata.BrainFuck.Automaton where

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Automaton  as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Automaton  as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Automaton  as Tree

import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType

simpleRun :: BIO m => (BFType , Source , CellType) -> m ()
simpleRun (c , s , t) = run c s t Pretty

----

runWithParams :: BIO m => BFType -> RunParams -> m ()
runWithParams b p = run b (source p) (cellTypeOptions p) (dumpTypeOptions p)

run :: BIO m => BFType -> Source -> CellType -> DumpType -> m ()
run c s Int8Type   = runSource c s (newTape :: FullTape Int8)
run c s Word8Type  = runSource c s (newTape :: FullTape Word8)
run c s Int16Type  = runSource c s (newTape :: FullTape Int16)
run c s Word16Type = runSource c s (newTape :: FullTape Word16)
run c s Int32Type  = runSource c s (newTape :: FullTape Int32)
run c s Word32Type = runSource c s (newTape :: FullTape Word32)
run c s Int64Type  = runSource c s (newTape :: FullTape Int64)
run c s Word64Type = runSource c s (newTape :: FullTape Word64)

runSource :: (BIO m , Symbol e) => BFType -> Source -> FullTape e -> DumpType -> m ()
runSource FastType = Fast.runSource
runSource TreeType = Tree.runSource
runSource FlatType = Flat.runSource
