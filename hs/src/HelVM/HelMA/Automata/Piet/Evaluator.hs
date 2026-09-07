module HelVM.HelMA.Automata.Piet.Evaluator
  ( emitCommands
  , emitDot
  , emitIL
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.AssemblyGenerator
import           HelVM.HelMA.Automata.Piet.Compiler
import           HelVM.HelMA.Automata.Piet.ImageReader
import           HelVM.HelMA.Automata.Piet.InstructionCompiler
import           HelVM.HelMA.Automata.Piet.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxParser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.Program
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.API.CodelSize
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.PietOptions

import qualified HelVM.HelMA.Automata.Piet.Automaton.Collision as Collision
import qualified HelVM.HelMA.Automata.Piet.Automaton.StepState as StepState

import           HelVM.HelMA.Automaton.Instruction

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

type ImageInput = (ImageConfig, DynamicImage)

runRio ∷ Has env ⇒ PietOptions → RIO.RIO env ()
runRio po = runWithOptions po =<< optionsRio

runWithOptions ∷ Has env ⇒ PietOptions → App.AppOptions → RIO.RIO env ()
runWithOptions po o = run (App.emit o) po =<< readImageRio (App.file o)

run ∷ Has env ⇒ Emit → PietOptions → DynamicImage → RIO.RIO env ()
run No po = runAsRIO . simpleEval po.implType po.codelSize
run IL po =  putLTextLnRio <=< (runAsRIO . emitIL . imageInput po)
run TL po = putLTextLnRio <=< (runAsRIO . emitCommands . imageInput po)
run _ po  = putLTextLnRio <=< (runAsRIO . emitDot . imageInput po)

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe CodelSize → DynamicImage → m ()
simpleEval i cs = start i . uncurry compile <=< logCS . processImage cs

imageInput ∷ PietOptions → DynamicImage → ImageInput
imageInput po dyn = (imageConfig po, dyn)

emitIL ∷ MonadSafe m ⇒ ImageInput → m LText
emitIL = fmap (printIL . compileToIL . generateAssembly) . parseColors

emitCommands ∷ MonadSafe m ⇒ ImageInput → m LText
emitCommands = fmap (renderAssembly . generateAssembly) . parseColors

emitDot ∷ MonadSafe m ⇒ ImageInput → m LText
emitDot = fmap syntaxToDOT . parseColors

-- HELPERS

start ∷ AppSafeEff m ⇒ ImplType → Program → m ()
start StepState = StepState.start
start Collision = Collision.start

logCS ∷ MonadLogger m ⇒ (CodelSizeInternal, Grid Color) → m (CodelSizeInternal, Grid Color)
logCS res@(cs, _) = logDebugN ("Actual codel length: " <> show cs) $> res

parseColors ∷ MonadSafe m ⇒ ImageInput → m (Maybe SyntaxGraph)
parseColors = parse <=< uncurry readColors
