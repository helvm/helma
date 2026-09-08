module HelVM.HelMA.Automata.Piet.Evaluator
  ( emitCommands
  , emitDot
  , emitIL
  , evalCustom
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
import           HelVM.HelMA.Automaton.API.EvalOptions         ( EvalOptions, automatonOptions, parserOptions, simpleEvalOptions )
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
runRio po = runWithOptions po =<< optionsRio

runWithOptions ∷ Has env ⇒ Options → App.AppOptions → RIO.RIO env ()
runWithOptions po o = run (App.emit o) (App.evalOptions o) po =<< readImageRio (App.file o)

run ∷ Has env ⇒ Emit → EvalOptions → Options → DynamicImage → RIO.RIO env ()
run No eo po = runAsRIO . evalParams (fromMaybe Custom (automatonType po)) eo po
run IL eo po = putLTextLnRio <=< (runAsRIO . emitIL (optLevel $ parserOptions eo) . imageInput po)
run TL _  po = putLTextLnRio <=< (runAsRIO . emitCommands . imageInput po)
run _  _  po = putLTextLnRio <=< (runAsRIO . emitDot . imageInput po)

evalParams ∷ AppSafeEff m ⇒ AutomatonType → EvalOptions → Options → DynamicImage → m ()
evalParams Custom _  po = evalCustom po.implType po.codelSize
evalParams _      eo po = evalCommon eo po

simpleEval ∷ AppSafeEff m ⇒ DynamicImage → m ()
simpleEval = evalCommon simpleEvalOptions simplePietOptions

evalCommon ∷ AppSafeEff m ⇒ EvalOptions → Options → DynamicImage → m ()
evalCommon eo po = flip Automaton.start (automatonOptions eo) <=< generateIL (optLevel $ parserOptions eo) . imageInput po

evalCustom ∷ AppSafeEff m ⇒ ImplType → Maybe CodelSize → DynamicImage → m ()
evalCustom i cs = start i . uncurry compile <=< logCS . processImage cs

emitIL ∷ MonadSafe m ⇒ OptimizationLevel → ImageInput → m LText
emitIL ol = fmap printIL . generateIL ol

generateIL ∷ MonadSafe m ⇒ OptimizationLevel → ImageInput → m InstructionList
generateIL ol = fmap (aaa ol) . parseColors

aaa ∷ OptimizationLevel → Maybe SyntaxGraph → InstructionList
aaa ol = optimize ol . compileToIL . generateAssembly

imageInput ∷ Options → DynamicImage → ImageInput
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
