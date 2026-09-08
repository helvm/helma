module HelVM.HelMA.Automata.Piet.Evaluator
  ( emitCommands
  , emitDot
  , emitIL
  , evalCustom
  , run
  , runRio
  , simpleEval
  , simpleEvalCustom
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
import           HelVM.HelMA.Automata.Piet.API.Options

import qualified HelVM.HelMA.Automata.Piet.Automaton.Collision as Collision
import qualified HelVM.HelMA.Automata.Piet.Automaton.StepState as StepState

import qualified HelVM.HelMA.Automaton.Automaton               as Automaton
import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Optimizer

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import           HelVM.HelMA.Automaton.API.AutomatonType
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalOptions
import           HelVM.HelMA.Automaton.API.OptimizationLevel
import           HelVM.HelMA.Automaton.API.ParserOptions

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

type ImageInput = (ImageConfig, DynamicImage)

runRio ∷ Has env ⇒ Options → RIO.RIO env ()
runRio o = runWithOptions o =<< optionsRio

runWithOptions ∷ Has env ⇒ Options → App.AppOptions → RIO.RIO env ()
runWithOptions o ao = run (App.emit ao) (App.evalOptions ao) o =<< readImageRio (App.file ao)

run ∷ Has env ⇒ Emit → EvalOptions → Options → DynamicImage → RIO.RIO env ()
run No eo o = runAsRIO . evalParams (fromMaybe Custom (automatonType o)) eo o
run IL eo o = putLTextLnRio <=< (runAsRIO . emitIL (optLevel $ parserOptions eo) . imageInput o)
run TL _  o = putLTextLnRio <=< (runAsRIO . emitCommands . imageInput o)
run _  _  o = putLTextLnRio <=< (runAsRIO . emitDot . imageInput o)

evalParams ∷ AppSafeEff m ⇒ AutomatonType → EvalOptions → Options → DynamicImage → m ()
evalParams Common eo = evalCommon eo
evalParams Custom _  = evalCustom

simpleEval ∷ AppSafeEff m ⇒ DynamicImage → m ()
simpleEval = evalCommon simpleEvalOptions simplePietOptions

simpleEvalCustom ∷ AppSafeEff m ⇒ (ImplType , Maybe CodelSize) → DynamicImage → m ()
simpleEvalCustom = evalCustom . simplePietOptions2

evalCommon ∷ AppSafeEff m ⇒ EvalOptions → Options → DynamicImage → m ()
evalCommon eo o = flip Automaton.start (automatonOptions eo) <=< generateIL (optLevel $ parserOptions eo) . imageInput o

evalCustom ∷ AppSafeEff m ⇒ Options → DynamicImage → m ()
evalCustom o = start o.implType . uncurry compile <=< logCS . processImage o.codelSize

emitIL ∷ MonadSafe m ⇒ OptimizationLevel → ImageInput → m LText
emitIL ol = fmap printIL . generateIL ol

generateIL ∷ MonadSafe m ⇒ OptimizationLevel → ImageInput → m InstructionList
generateIL ol = fmap (buildIL ol) . parseColors

buildIL ∷ OptimizationLevel → Maybe SyntaxGraph → InstructionList
buildIL ol = optimize ol . compileToIL . generateAssembly

imageInput ∷ Options → DynamicImage → ImageInput
imageInput o dyn = (imageConfig o, dyn)

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
