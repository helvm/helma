module HelVM.HelMA.Automata.Piet.Types.Coordinates
  ( Block
  , Coordinates
  , initialCoordinates
  , neighbours
  ) where

import qualified Relude.Extra as Extra

type Block = [Coordinates]

neighbours ∷ Coordinates → [Coordinates]
neighbours (x, y) = [(x, Extra.prev y), (x, Extra.next y), (Extra.prev x, y), (Extra.next x, y)]

initialCoordinates ∷ Coordinates
initialCoordinates = (0, 0)

type Coordinates = (Int , Int)
