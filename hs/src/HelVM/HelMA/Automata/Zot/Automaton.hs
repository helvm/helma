module HelVM.HelMA.Automata.Zot.Automaton (
  evalParams,
  evalWithFormat,
) where

import           HelVM.HelMA.Automata.Zot.Evaluator
import           HelVM.HelMA.Automata.Zot.Expression
import           HelVM.HelMA.Automata.Zot.Parser

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Types.FormatType

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Writer.Lazy

import qualified Data.Text.Lazy                         as LT

evalParams :: BIO m => EvalParams -> m ()
evalParams p = wPutStr =<< evalWithFormat (formatType p) (source p) =<< wGetContentsText

evalWithFormat :: MonadSafe m => FormatType -> Source -> LT.Text -> m Output
evalWithFormat BinaryLabel source input = pure $ showFoldable $ evalInternal source input
evalWithFormat TextLabel   source input = (makeAsciiText28 . convert . evalInternal source) . showExpressionList =<< stringToDL (toString input)

evalInternal :: Source -> LT.Text -> ExpressionDList
evalInternal source input = eval $ fromStrict source <> input

eval :: LT.Text  -> ExpressionDList
eval = execWriter . runExpressionList . parse
