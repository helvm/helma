module HelVM.HelMA.Automata.Piet.Evaluator
  ( run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.Compiler
import           HelVM.HelMA.Automata.Piet.Parser

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.Program

import           HelVM.HelMA.Automata.Piet.API.ImplType
import           HelVM.HelMA.Automata.Piet.API.LexerType

import           HelVM.HelMA.Automata.Piet.Automaton.Hi       as Hi
import           HelVM.HelMA.Automata.Piet.Automaton.Original as Original

import qualified HelVM.HelMA.Automaton.API.AppOptions         as App
import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import qualified Codec.Picture                                as Picture

import           Control.Monad.Logger

import qualified RIO

runRio ∷ Has env ⇒ ImplType → Maybe Natural → Maybe LexerType → RIO.RIO env ()
runRio  implType codelInfo _  = runWithOptions  =<< optionsRio where
  runWithOptions o = run implType codelInfo =<< readImageRio (App.file o)

run ∷ Has env ⇒  ImplType → Maybe Natural → Picture.DynamicImage → RIO.RIO env ()
run  implType codelInfo = runAsRIO . simpleEval implType codelInfo

simpleEval ∷ AppSafeEff m ⇒ ImplType → Maybe Natural → Picture.DynamicImage → m ()
simpleEval implType codelInfo dynamicImage = (interpret implType . uncurry compile) =<< logCS (processImage codelInfo dynamicImage)

interpret ∷ AppSafeEff m ⇒ ImplType → Program → m ()
interpret Original = Original.start
interpret Hi       = Hi.start

logCS ∷ MonadLogger m ⇒ (CodelSize, Image Color) → m (CodelSize, Image Color)
logCS (cs , img) = (cs , img) <$ logDebugN ("Actual codel length: " <> show cs)
