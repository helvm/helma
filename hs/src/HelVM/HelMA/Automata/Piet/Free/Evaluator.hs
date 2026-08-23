module HelVM.HelMA.Automata.Piet.Free.Evaluator
  ( interpret
  , run
  , runRio
  , simpleEval
  ) where

import           HelVM.HelMA.Automata.Piet.Free.Automaton
import           HelVM.HelMA.Automata.Piet.Parser

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Image         ( Image )
import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig
import           HelVM.HelMA.Automata.Piet.Types.ProgramState

import           HelVM.HelMA.Automata.Piet.API.LexerType

import qualified HelVM.HelMA.Automaton.API.AppOptions          as App
import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import qualified Codec.Picture                                 as Picture

import qualified RIO

runRio ∷ Has env ⇒ Maybe LexerType → Natural → RIO.RIO env ()
runRio _ codelInfo = runWithOptions =<< optionsRio where
  runWithOptions o = run codelInfo =<< readImageRio (App.file o)

run ∷ Has env ⇒ Natural → Picture.DynamicImage → RIO.RIO env ()
run cl i = runAsRIO $ simpleEval cl i

simpleEval ∷ AppSafeEff m ⇒ Natural → Picture.DynamicImage → m ()
simpleEval cs = interpret (fromIntegral cs) . parseColorImage cs

interpret ∷ AppSafeEff m ⇒ CodelSize → Image Color → m ()
interpret cs img = loop initialState where
  conf = ProgramConfig { _codelSize = cs, _image = img }
  loop st = do
    (continue, st') <- transition conf st
    when continue $ loop st'
