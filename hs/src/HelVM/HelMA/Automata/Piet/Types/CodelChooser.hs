module HelVM.HelMA.Automata.Piet.Types.CodelChooser
  ( CodelChooser (..)
  , nextChooser
  , toggle
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

nextChooser ∷ CodelChooser → CodelChooser
nextChooser CCLeft  = CCRight
nextChooser CCRight = CCLeft


toggle ∷ Int → CodelChooser → CodelChooser
toggle = change 2

data CodelChooser
  = CCLeft
  | CCRight
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)
