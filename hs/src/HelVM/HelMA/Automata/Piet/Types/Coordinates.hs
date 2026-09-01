module HelVM.HelMA.Automata.Piet.Types.Coordinates
  ( Block
  , BlockCoordinates
  , Coordinates
  , initialCoordinates
  , neighbours
  , nextCoords
  ) where

import           Relude.Extra

type BlockCoordinates = [Coordinates]
type Block = BlockCoordinates

neighbours ∷ Coordinates → BlockCoordinates
neighbours (x, y) = [(x, prev y), (x, next y), (prev x, y), (next x, y)]

initialCoordinates ∷ Coordinates
initialCoordinates = (0, 0)

type Coordinates = (Int , Int)

nextCoords ∷ Coordinates → Coordinates → Maybe Coordinates
nextCoords (w, h) (cx, cy) = guardX (cx < w - 1) cx cy h

guardX ∷ Bool → Int → Int → Int → Maybe Coordinates
guardX False _  cy h = guardY (cy < h - 1) cy
guardX True  cx cy _ = Just (cx + 1, cy)

guardY ∷ Bool → Int → Maybe Coordinates
guardY False _  = Nothing
guardY True  cy = Just (0, cy + 1)
