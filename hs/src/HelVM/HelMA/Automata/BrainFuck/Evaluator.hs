module HelVM.HelMA.Automata.BrainFuck.Evaluator where

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Evaluator  as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser     as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator  as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser     as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator  as Tree
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser     as Tree


import           HelVM.HelMA.Automata.BrainFuck.API.BFType

import           HelVM.HelMA.Automata.BrainFuck.Common.Symbol
import           HelVM.HelMA.Automata.BrainFuck.Common.TapeOfSymbols

import qualified HelVM.HelMA.Automaton.API.AppOptions                as App
import qualified HelVM.HelMA.Automaton.API.Emit                      as Emit
import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.CellType
import           HelVM.HelMA.Automaton.Types.DumpType

import qualified RIO

import           Text.Pretty.Simple

runWithOptions :: Has env => App.AppOptions -> RIO.RIO env ()
runWithOptions o = runAsRIO . run (App.emit o) (App.bfType o) . App.evalParams o =<< readSourceFile (App.exec o) (App.file o)

run :: AppEff m => Emit.Emit -> BFType -> EvalParams -> m ()
run Emit.No   i        = evalParams i
run Emit.IL   FastType = ePutLTextLn . pShowNoColor . Fast.parseAsListSafe   . source
run Emit.IL   TreeType = ePutLTextLn . pShowNoColor . Tree.parseAsVectorSafe . source
run _ _                = ePutTextLn . show . Flat.readTokens . source

simpleEval :: AppEff m => (BFType , Source , CellType) -> m ()
simpleEval (c , s , t) = eval c s t Pretty --TODO Add MaybeLimit and use Trampoline

----

evalParams :: AppEff m => BFType -> EvalParams -> m ()
evalParams b p = eval b (source p) (cellAutoOptions p) (dumpAutoOptions p)

eval :: AppEff m => BFType -> Source -> CellType -> DumpType -> m ()
eval c s Int8Type   = evalSource c s (newTape :: FullTape Int8)
eval c s Word8Type  = evalSource c s (newTape :: FullTape Word8)
eval c s Int16Type  = evalSource c s (newTape :: FullTape Int16)
eval c s Word16Type = evalSource c s (newTape :: FullTape Word16)
eval c s Int32Type  = evalSource c s (newTape :: FullTape Int32)
eval c s Word32Type = evalSource c s (newTape :: FullTape Word32)
eval c s Int64Type  = evalSource c s (newTape :: FullTape Int64)
eval c s Word64Type = evalSource c s (newTape :: FullTape Word64)

evalSource :: (AppEff m , Symbol e) => BFType -> Source -> FullTape e -> DumpType -> m ()
evalSource FastType = Fast.evalSource
evalSource TreeType = Tree.evalSource
evalSource FlatType = Flat.evalSource
