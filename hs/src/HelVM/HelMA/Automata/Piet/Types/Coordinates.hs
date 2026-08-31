module HelVM.HelMA.Automata.Piet.Types.Coordinates
  ( Block
  , Coordinates
  , initialCoordinates
  , neighbours
  , nextCoords
  ) where

import qualified Relude.Extra as Extra

type Block = [Coordinates]

neighbours ∷ Coordinates → [Coordinates]
neighbours (x, y) = [(x, Extra.prev y), (x, Extra.next y), (Extra.prev x, y), (Extra.next x, y)]

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

