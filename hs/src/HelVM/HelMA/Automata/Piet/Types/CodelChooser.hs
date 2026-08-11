module HelVM.HelMA.Automata.Piet.Types.CodelChooser
  ( CodelChooser (..)
  , toggle
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

toggle ∷ Int → CodelChooser → CodelChooser
toggle = change 2

data CodelChooser = CCLeft | CCRight
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
