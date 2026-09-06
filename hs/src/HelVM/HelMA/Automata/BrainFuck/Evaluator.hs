module HelVM.HelMA.Automata.BrainFuck.Evaluator where

import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Evaluator  as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Fast.Parser     as Fast
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Evaluator  as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Parser     as Flat
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Evaluator  as Tree
import qualified HelVM.HelMA.Automata.BrainFuck.Impl.Tree.Parser     as Tree


import           HelVM.HelMA.Automata.BrainFuck.API.ImplType

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

import           HelVM.HelIO.Control.Safe

import           Control.Applicative.Tools

import qualified RIO

import           Text.Pretty.Simple

runRio ∷ Has env ⇒ ImplType → RIO.RIO env ()
runRio t = runWithOptions =<< optionsRio where
  runWithOptions o = run (App.emit o) t . App.evalParams o =<< readSourceFileRio

run ∷ Has env ⇒ Emit.Emit → ImplType → EvalParams → RIO.RIO env ()
run Emit.No i = runAsRIO . evalParams i
run Emit.IL i = putLTextLnRio <=< runAsRIO . emitIL i True . source
run Emit.TL _ = putLTextLnRio . emitTL . source
run _ _       = putLTextLnRio . emitCode . source

emitIL ∷ MonadSafe m ⇒ ImplType → Bool → Source → m LText
emitIL FastType True = pShowNoColor <.> Fast.parseWithOptimize
emitIL FastType _    = pShowNoColor <.> Fast.parseAsList
emitIL TreeType _    = pShowNoColor <.> Tree.parseAsVector
emitIL _        _    = pure . emitTL

emitTL ∷ Source → LText
emitTL = pShowNoColor . Flat.tokenize

emitCode ∷ Source → LText
emitCode = show . Flat.readTokens

simpleEval ∷ AppSafeEff m ⇒ (ImplType , Source , CellType) → m ()
simpleEval (c , s , t) = eval c s t Pretty --TODO Add MaybeLimit and use Trampoline

----

evalParams ∷ AppSafeEff m ⇒ ImplType → EvalParams → m ()
evalParams b p = eval b (source p) (cellAutoOptions p) (dumpAutoOptions p)

eval ∷ AppSafeEff m ⇒ ImplType → Source → CellType → DumpType → m ()
eval c s Int8Type   = evalSource c s (newTape :: FullTape Int8)
eval c s Word8Type  = evalSource c s (newTape :: FullTape Word8)
eval c s Int16Type  = evalSource c s (newTape :: FullTape Int16)
eval c s Word16Type = evalSource c s (newTape :: FullTape Word16)
eval c s Int32Type  = evalSource c s (newTape :: FullTape Int32)
eval c s Word32Type = evalSource c s (newTape :: FullTape Word32)
eval c s Int64Type  = evalSource c s (newTape :: FullTape Int64)
eval c s Word64Type = evalSource c s (newTape :: FullTape Word64)

evalSource ∷ (AppSafeEff m , Symbol e) ⇒ ImplType → Source → FullTape e → DumpType → m ()
evalSource FastType = Fast.evalSource
evalSource TreeType = Tree.evalSource
evalSource FlatType = Flat.evalSource
