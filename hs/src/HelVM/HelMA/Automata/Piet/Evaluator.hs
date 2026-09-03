module HelVM.HelMA.Automata.Piet.Evaluator
  ( run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.Compiler
import           HelVM.HelMA.Automata.Piet.Parser

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Grid
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType

import qualified HelVM.HelMA.Automata.Piet.Automaton.Collision as Collision
import qualified HelVM.HelMA.Automata.Piet.Automaton.StepState as StepState

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           Codec.Picture

import           Control.Monad.Logger

import qualified RIO

runRio ∷ Has env ⇒ ImplType → Maybe Natural → Maybe LexerType → RIO.RIO env ()
runRio  implType codelInfo _  = runWithOptions  =<< optionsRio where
  runWithOptions o = run implType codelInfo =<< readImageRio (App.file o)

run ∷ Has env ⇒  ImplType → Maybe Natural → DynamicImage → RIO.RIO env ()
run  implType codelInfo = runAsRIO . simpleEval implType codelInfo

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe Natural → DynamicImage → m ()
simpleEval implType codelInfo dynamicImage = (start implType . uncurry compile) =<< logCS (processImage codelInfo dynamicImage)

start ∷ AppSafeEff m ⇒ ImplType → Program → m ()
start StepState = StepState.start
start Collision = Collision.start

logCS ∷ MonadLogger m ⇒ (CodelSize, Grid Color) → m (CodelSize, Grid Color)
logCS (cs , img) = (cs , img) <$ logDebugN ("Actual codel length: " <> show cs)
