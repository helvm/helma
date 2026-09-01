module HelVM.HelMA.Automata.Piet.Types.CodelChooser
  ( CodelChooser (..)
  , charCC
  , nextChooser
  , toggle
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Cyclic

-- TYPES

data CodelChooser
  = CCLeft
  | CCRight
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

-- FUNCTIONS

nextChooser ∷ CodelChooser → CodelChooser
nextChooser CCLeft  = CCRight
nextChooser CCRight = CCLeft

toggle ∷ Int → CodelChooser → CodelChooser
toggle = change 2

charCC ∷ CodelChooser → Char
charCC CCLeft  = 'l'
charCC CCRight = 'r'
