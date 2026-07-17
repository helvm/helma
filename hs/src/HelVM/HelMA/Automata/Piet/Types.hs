module HelVM.HelMA.Automata.Piet.Types (
  DirectionPointer(..),
  addCoordinates,
  rotate,
  CodelChooser(..),
  toggle,
  PietType(..),
  Program(..),
  isBlocked,
) where

import           HelVM.HelMA.Automata.Piet.Color
import           HelVM.HelMA.Automata.Piet.Coordinates
import           HelVM.HelMA.Automata.Piet.Image
import           HelVM.HelMA.Automata.Piet.Label

import qualified Relude.Extra                          as Extra

data DirectionPointer = DPRight | DPDown | DPLeft | DPUp
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

addCoordinates :: DirectionPointer -> Int -> Int -> Coordinates
addCoordinates DPRight x y = (x + 1, y)
addCoordinates DPDown  x y = (x, y + 1)
addCoordinates DPLeft  x y = (x - 1, y)
addCoordinates DPUp    x y = (x, y - 1)

rotate :: Int -> DirectionPointer -> DirectionPointer
rotate n dp
  | n < 0     = rotate (n `mod` 4) dp
  | n == 0    = dp
  | otherwise = appEndo (stimes n (Endo Extra.next)) dp

data CodelChooser = CCLeft | CCRight
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

toggle :: Int -> CodelChooser -> CodelChooser
toggle n cc
  | odd n     = Extra.next cc
  | otherwise = cc

data PietType = PietNumber | PietChar
  deriving stock (Show, Read, Eq, Ord)

data Program = Program
  { image :: Image Color
  , mask  :: Image LabelKey
  , info  :: IntMap (Maybe LabelInfo)
  }

isBlocked :: Coordinates -> Program -> Bool
isBlocked (x, y) p = not (imgInRange (x, y) $ image p) || (Black == imgPixel (x, y) (image p))
