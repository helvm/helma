module HelVM.HelMA.Automata.Piet.Evaluator
  ( run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.Automaton
import           HelVM.HelMA.Automata.Piet.Compiler
import           HelVM.HelMA.Automata.Piet.Parser

import           HelVM.HelMA.Automata.Piet.API.LexerType

import qualified HelVM.HelMA.Automaton.API.AppOptions    as App
import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import qualified Codec.Picture                           as Picture

import qualified RIO

runRio ∷ Has env ⇒ Maybe LexerType → Maybe Natural → RIO.RIO env ()
runRio  _ codelInfo = runWithOptions =<< optionsRio where
  runWithOptions o = run codelInfo =<< readImageRio (App.file o)

run ∷ Has env ⇒  Maybe Natural → Picture.DynamicImage → RIO.RIO env ()
run cl i = runAsRIO $ simpleEval cl i

simpleEval ∷ AppSafeEff m ⇒ Maybe Natural → Picture.DynamicImage → m ()
simpleEval codelInfo dynamicImage = (interpret . compile) =<< processImage codelInfo dynamicImage
