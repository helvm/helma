module HelVM.HelMA.Automata.Piet.Types.CodelChooser (
  toggle,
  CodelChooser(..),
) where

import           HelVM.HelMA.Automata.Piet.Types.Extra

toggle :: Int -> CodelChooser -> CodelChooser
toggle = change 2

data CodelChooser = CCLeft | CCRight
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)
