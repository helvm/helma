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

import qualified HelVM.HelMA.Automaton.Automaton               as Automaton
import           HelVM.HelMA.Automaton.Instruction

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import           HelVM.HelMA.Automaton.API.AutomatonType
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalOptions         ( EvalOptions )
import qualified HelVM.HelMA.Automaton.API.EvalOptions         as EvalOptions

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
runWithOptions po o = run (App.emit o) (App.evalOptions o) po =<< readImageRio (App.file o)

run ∷ Has env ⇒ Emit → EvalOptions → PietOptions → DynamicImage → RIO.RIO env ()
run No eo po = runAsRIO . simpleEvalByType (fromMaybe Custom (automatonType po)) eo po
run IL _  po = putLTextLnRio <=< (runAsRIO . emitIL . imageInput po)
run TL _  po = putLTextLnRio <=< (runAsRIO . emitCommands . imageInput po)
run _  _  po = putLTextLnRio <=< (runAsRIO . emitDot . imageInput po)

simpleEvalByType ∷ AppSafeEff m ⇒ AutomatonType → EvalOptions → PietOptions → DynamicImage → m ()
simpleEvalByType Custom _  po = simpleEval po.implType po.codelSize
simpleEvalByType _      eo po = simpleEvalCommon eo po

simpleEvalCommon ∷ AppSafeEff m ⇒ EvalOptions → PietOptions → DynamicImage → m ()
simpleEvalCommon eo po = flip Automaton.start (EvalOptions.automatonOptions eo) <=< generateIL . imageInput po

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe CodelSize → DynamicImage → m ()
simpleEval i cs = start i . uncurry compile <=< logCS . processImage cs

emitIL ∷ MonadSafe m ⇒ ImageInput → m LText
emitIL = fmap printIL . generateIL

generateIL ∷ MonadSafe m ⇒ ImageInput → m InstructionList
generateIL = fmap (compileToIL . generateAssembly) . parseColors

imageInput ∷ PietOptions → DynamicImage → ImageInput
imageInput po dyn = (imageConfig po, dyn)

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
