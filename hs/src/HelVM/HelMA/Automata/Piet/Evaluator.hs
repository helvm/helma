module HelVM.HelMA.Automata.Piet.Evaluator
  ( assemblyText
  , graphText
  , ilText
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

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.CodelSize
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

import qualified HelVM.HelMA.Automata.Piet.Automaton.Collision           as Collision
import qualified HelVM.HelMA.Automata.Piet.Automaton.StepState           as StepState

import           HelVM.HelMA.Automaton.Instruction

import qualified HelVM.HelMA.Automaton.API.AppOptions                    as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

type ImageInput = (ImageConfig, DynamicImage)

runRio ∷ Has env ⇒ ImplType → Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → Maybe LexerType → RIO.RIO env ()
runRio i a m cs _ = runWithOptions i a m cs =<< optionsRio

run ∷ Has env ⇒ Emit → ImplType → Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → DynamicImage → RIO.RIO env ()
run No i _ _ cs = runAsRIO . simpleEval i cs
run IL _ a m cs = putLTextLnRio <=< (runAsRIO . ilText . imageInput a m cs)
run TL _ a m cs = putLTextLnRio <=< (runAsRIO . assemblyText . imageInput a m cs)
run _  _ a m cs = putLTextLnRio <=< (runAsRIO . graphText . imageInput a m cs)

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe CodelSize → DynamicImage → m ()
simpleEval i cs = start i . uncurry compile <=< logCS . processImage cs

ilText ∷ MonadSafe m ⇒ ImageInput → m LText
ilText = fmap (renderIL . compileToIL . generateAssembly) . parseColors

assemblyText ∷ MonadSafe m ⇒ ImageInput → m LText
assemblyText = fmap (renderAssembly . generateAssembly) . parseColors

graphText ∷ MonadSafe m ⇒ ImageInput → m LText
graphText = fmap syntaxToDOT . parseColors

-- HELPERS

runWithOptions ∷ Has env ⇒ ImplType → Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → App.AppOptions → RIO.RIO env ()
runWithOptions i a m cs o = run (App.emit o) i a m cs =<< readImageRio (App.file o)

start ∷ AppSafeEff m ⇒ ImplType → Program → m ()
start StepState = StepState.start
start Collision = Collision.start

logCS ∷ MonadLogger m ⇒ (CodelSizeInternal, Grid Color) → m (CodelSizeInternal, Grid Color)
logCS res@(cs, _) = logDebugN ("Actual codel length: " <> show cs) $> res

parseColors ∷ MonadSafe m ⇒ ImageInput → m (Maybe SyntaxGraph)
parseColors = parse <=< uncurry readColors

imageInput ∷ Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → DynamicImage → ImageInput
imageInput a m cs dyn = (imageConfig a m cs, dyn)

imageConfig ∷ Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → ImageConfig
imageConfig a m = ImageConfig (fromMaybe defaultAdditionalColorStrategy a) (fromMaybe defaultMulticoloredCodelStrategy m)
