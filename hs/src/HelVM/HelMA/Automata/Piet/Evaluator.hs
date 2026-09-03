module HelVM.HelMA.Automata.Piet.Evaluator
  ( graphText
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.Compiler
import           HelVM.HelMA.Automata.Piet.ImageReader
import           HelVM.HelMA.Automata.Piet.Parser
import           HelVM.HelMA.Automata.Piet.SyntaxParser
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer


import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.CodelSize
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

import qualified HelVM.HelMA.Automata.Piet.Automaton.Collision           as Collision
import qualified HelVM.HelMA.Automata.Piet.Automaton.StepState           as StepState

import qualified HelVM.HelMA.Automaton.API.AppOptions                    as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelIO.Control.Safe

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

runRio ∷ Has env ⇒ ImplType → Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → Maybe LexerType → RIO.RIO env ()
runRio i a m codelInfo _  = runWithOptions =<< optionsRio where
  runWithOptions o = run (App.emit o) i a m codelInfo =<< readImageRio (App.file o)

run ∷ Has env ⇒ Emit → ImplType → Maybe AdditionalColorStrategy → Maybe MulticoloredCodelStrategy → Maybe CodelSize → DynamicImage → RIO.RIO env ()
run No i _ _ codelInfo = runAsRIO . simpleEval i codelInfo
run _  _ a m codelInfo = putLTextLnRio <=< (runAsRIO . graphText config) where
  config =  ImageConfig a' m' codelInfo
  a' = fromMaybe defaultAdditionalColorStrategy a
  m' = fromMaybe defaultMulticoloredCodelStrategy m

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe CodelSize → DynamicImage → m ()
simpleEval implType codelInfo dynamicImage = (start implType . uncurry compile) =<< logCS (processImage codelInfo dynamicImage)

start ∷ AppSafeEff m ⇒ ImplType → Program → m ()
start StepState = StepState.start
start Collision = Collision.start

logCS ∷ MonadLogger m ⇒ (CodelSizeInternal, Grid Color) → m (CodelSizeInternal, Grid Color)
logCS (cs , img) = (cs , img) <$ logDebugN ("Actual codel length: " <> show cs)

graphText ∷ MonadSafe m ⇒ ImageConfig → DynamicImage → m LText
graphText imageConfig image = syntaxToDOT <$> (parse =<< readColors imageConfig image)
