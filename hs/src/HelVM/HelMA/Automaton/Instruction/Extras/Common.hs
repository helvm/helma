module HelVM.HelMA.Automaton.Instruction.Extras.Common where

import qualified Data.Text as Text

type ImmediateIndex = Int

toLowerShow ∷ Show i ⇒ i → Text
toLowerShow = Text.toLower . show
