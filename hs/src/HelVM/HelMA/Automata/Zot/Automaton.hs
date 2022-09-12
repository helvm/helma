module HelVM.HelMA.Automata.Zot.Automaton (
  runWithParams,
  run,
) where

import           HelVM.HelMA.Automata.Zot.Evaluator
import           HelVM.HelMA.Automata.Zot.Expression
import           HelVM.HelMA.Automata.Zot.Parser

import           HelVM.HelMA.Automaton.API.IOTypes
import           HelVM.HelMA.Automaton.API.RunParams

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Writer.Lazy

import qualified Data.Text.Lazy                      as LT

runWithParams :: BIO m => RunParams -> m ()
runWithParams p = wPutStr =<< run (asciiLabel p) (source p) =<< wGetContentsText

run :: MonadSafe m => Bool -> Source -> LT.Text -> m Output
run False source input = pure $ showFoldable $ run2 source input
run True  source input = (makeAsciiText28 . convert . run2 source) . showExpressionList =<< stringToDL (toString input)

run2 :: Source -> LT.Text  -> ExpressionDList
run2 source input = evalText $ fromStrict source <> input

evalText :: LT.Text  -> ExpressionDList
evalText = execWriter . evalExpressionList . parse
