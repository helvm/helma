module HelVM.HelMA.Automata.Zot.Evaluator (
  runRio,
  run,
  evalParams,
  evalWithFormat,
) where

import           HelVM.HelMA.Automata.Zot.Automaton
import           HelVM.HelMA.Automata.Zot.Expression
import           HelVM.HelMA.Automata.Zot.Parser

import qualified HelVM.HelMA.Automaton.API.AppOptions  as App
import           HelVM.HelMA.Automaton.API.Emit
import           HelVM.HelMA.Automaton.API.Env
import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Extra

import           HelVM.HelMA.Automaton.Types.LabelType

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Writer.Lazy

import qualified RIO

runRio :: Has env => RIO.RIO env ()
runRio = runWithOptions =<< optionsRio where
  runWithOptions o =  run (App.emit o) . App.evalParams o =<< readSourceFileRio

run :: Has env =>  Emit -> EvalParams -> RIO.RIO env ()
run No = runAsRIO . evalParams
run _  = fallback

evalParams :: AppEff m => EvalParams -> m ()
evalParams p = putTextEff =<< evalWithFormat (formatType p) (source p) =<< getContentsText

evalWithFormat :: MonadSafe m => LabelType -> Source -> LText -> m Output
evalWithFormat BinaryLabel source input = pure $ showFoldable $ evalInternal source input
evalWithFormat TextLabel   source input = (makeAsciiText28 . convert . evalInternal source) . showExpressionList =<< stringToDL (toString input)

evalInternal :: Source -> LText -> ExpressionDList
evalInternal source input = eval $ fromStrict source <> input

eval :: LText  -> ExpressionDList
eval = execWriter . runExpressionList . parse
