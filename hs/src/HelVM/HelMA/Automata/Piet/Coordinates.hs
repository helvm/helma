module HelVM.HelMA.Automata.Piet.Coordinates (
  addPixel,
  succCoordinates,
  previousNeighbours,
  nextCoordinates,
  Coordinates,
) where

import           HelVM.HelMA.Automata.Piet.FullInfo
import           HelVM.HelMA.Automata.Piet.LabelBorder
import           HelVM.HelMA.Automata.Piet.MovePointer

import           Data.Tuple.HT
import           Relude.Extra


--FIXME move to LabelInfo
addPixel :: Coordinates -> LabelInfo -> LabelInfo
addPixel = (<>) . (Just . toFullInfo)

toFullInfo :: Coordinates -> FullInfo
toFullInfo (x , y) = FullInfo
  { labelSize  = 1
  , labelTop  = LabelBorder y x x
  , labelLeft  = LabelBorder x y y
  , labelBottom  = LabelBorder y x x
  , labelRight  = LabelBorder x y y
  }

succCoordinates :: MovePointer -> FullInfo -> Coordinates
succCoordinates (dp , cc) label = addCoordinates dp $ mapPair ff $ dup label where
  ff = calculateCoordinates (dp , cc)

calculateCoordinates :: MovePointer -> (FullInfo -> Int, FullInfo -> Int)
calculateCoordinates (DPRight , CCLeft  ) = (borderCoord . labelRight  , borderMin   . labelRight )
calculateCoordinates (DPRight , CCRight ) = (borderCoord . labelRight  , borderMax   . labelRight )
calculateCoordinates (DPDown  , CCLeft  ) = (borderMax   . labelBottom , borderCoord . labelBottom)
calculateCoordinates (DPDown  , CCRight ) = (borderMin   . labelBottom , borderCoord . labelBottom)
calculateCoordinates (DPLeft  ,  CCLeft ) = (borderCoord . labelLeft   , borderMax   . labelLeft  )
calculateCoordinates (DPLeft  , CCRight ) = (borderCoord . labelLeft   , borderMin   . labelLeft  )
calculateCoordinates (DPUp    , CCLeft  ) = (borderMin   . labelTop    , borderCoord . labelTop   )
calculateCoordinates (DPUp    , CCRight ) = (borderMax   . labelTop    , borderCoord . labelTop   )

addCoordinates :: DirectionPointer -> Coordinates -> Coordinates
addCoordinates DPRight (x , y) = (x + 1, y)
addCoordinates DPDown  (x , y) = (x, y + 1)
addCoordinates DPLeft  (x , y) = (x - 1, y)
addCoordinates DPUp    (x , y) = (x, y - 1)

previousNeighbours :: Coordinates -> [Coordinates]
previousNeighbours (x , y) = filter (\(x' , y') -> 0 <= x' && 0 <= y') [ (x-1 , y) , (x , y-1) ]

nextCoordinates :: Coordinates -> Coordinates -> Maybe Coordinates
nextCoordinates (x0 , y0) (x , y)
  | x < x0 - 1  = Just (x + 1 , y)
  | y < y0 - 1  = Just (0 , y + 1)
  | otherwise   = Nothing

type Coordinates = (Int , Int)


