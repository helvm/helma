module HelVM.HelMA.Automata.Rev.Evaluator (
  evalParams,
  eval,
) where

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.Eff.MonadEff

import qualified Data.Text                            as Text

evalParams :: AppEff m => EvalParams -> m ()
evalParams = eval . source

eval :: MonadEff m => Source -> m ()
eval = evalLines . lines

evalLines :: MonadEff m => [Source] -> m ()
evalLines ll = doOutput $ unlines $ Text.reverse <$> ll

doOutput :: MonadEff m => Source -> m ()
doOutput = ePutText
