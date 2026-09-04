module HelVM.HelMA.Automaton.ShowList where

import qualified Data.Text.Lazy.Builder      as LText

import           HelVM.HelIO.Control.Message
import           HelVM.HelIO.Control.Safe


showListSafeToLText ∷ Show a ⇒ Safe [a] → LText
showListSafeToLText = either (toLText . errorsToText) showListToLText

printListSafeToLText ∷ (a → LText) → Safe [a] → LText
printListSafeToLText = either (toLText . errorsToText) . printListToLText

showListToLText ∷ Show a ⇒ [a] → LText
showListToLText = printListToLText show

printListToLText ∷ (a → LText) → [a] → LText
printListToLText f = buildListToLText (LText.fromLazyText  . f)

buildListToLText ∷ (a → LText.Builder) → [a] → LText
buildListToLText toBuilder = LText.toLazyText . foldMap (wrap toBuilder)

wrap ∷ (Semigroup a, IsString a) ⇒ (t → a) → t → a
wrap toBuilder x = toBuilder x <> "\n"
