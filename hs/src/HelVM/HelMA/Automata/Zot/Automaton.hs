module HelVM.HelMA.Automata.Zot.Automaton (
  evalParams,
  evalWithFormat,
) where

import           HelVM.HelMA.Automata.Zot.Evaluator
import           HelVM.HelMA.Automata.Zot.Expression
import           HelVM.HelMA.Automata.Zot.Parser

import           HelVM.HelMA.Automaton.API.EvalParams
import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Types.FormatType

import           HelVM.HelIO.Containers.Extra
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Digit.Digitable
import           HelVM.HelIO.Digit.ToDigit

import           HelVM.HelIO.ListLikeExtra

import           Control.Monad.Writer.Lazy

evalParams :: AppEff m => EvalParams -> m ()
evalParams p = ePutText =<< evalWithFormat (formatType p) (source p) =<< eGetContentsText

evalWithFormat :: MonadSafe m => FormatType -> Source -> LText -> m Output
evalWithFormat BinaryLabel source input = pure $ showFoldable $ evalInternal source input
evalWithFormat TextLabel   source input = (makeAsciiText28 . convert . evalInternal source) . showExpressionList =<< stringToDL (toString input)

evalInternal :: Source -> LText -> ExpressionDList
evalInternal source input = eval $ fromStrict source <> input

eval :: LText  -> ExpressionDList
eval = execWriter . runExpressionList . parse
