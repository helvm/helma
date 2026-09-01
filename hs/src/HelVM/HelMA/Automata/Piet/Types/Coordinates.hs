module HelVM.HelMA.Automata.Piet.Types.Coordinates
  ( BlockCoordinates
  , Coordinates
  , initialCoordinates
  , neighbours
  , nextCoords
  ) where

import           Relude.Extra

-- TYPES & ALIASES

type Coordinates = (Int, Int)
type BlockCoordinates = [Coordinates]

-- CONSTANTS

initialCoordinates ∷ Coordinates
initialCoordinates = (0, 0)

-- FUNCTIONS

neighbours ∷ Coordinates → BlockCoordinates
neighbours (x, y) = [(x, prev y), (x, next y), (prev x, y), (next x, y)]

nextCoords ∷ Coordinates → Coordinates → Maybe Coordinates
nextCoords (w, h) (cx, cy)
  | cx < w - 1 = Just (cx + 1, cy)
  | cy < h - 1 = Just (0, cy + 1)
  | otherwise  = Nothing
