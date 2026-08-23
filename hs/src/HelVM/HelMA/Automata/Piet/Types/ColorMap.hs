
module HelVM.HelMA.Automata.Piet.Types.ColorMap
  ( ColorMap (..)
  , mapHeight
  , mapWidth
  , matrix
  ) where
import           HelVM.HelMA.Automata.Piet.Types.Color

import qualified Data.Vector                           as Vector

import           Lens.Micro.Platform

data ColorMap
  = ColorMap
      { _matrix    :: Vector.Vector (Vector.Vector Color)
      , _mapWidth  :: Int
      , _mapHeight :: Int
      }
  deriving stock (Eq, Show)

makeLenses ''ColorMap
