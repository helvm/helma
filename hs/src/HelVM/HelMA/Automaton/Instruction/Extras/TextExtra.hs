module HelVM.HelMA.Automaton.Instruction.Extras.TextExtra where

import qualified Data.Text as Text

toLowerShow :: Show i => i -> Text
toLowerShow = Text.toLower . show
