
module HelVM.HelMA.Automata.Piet.Types.ColorMap
  ( ColorMap (..)
  , mapHeight
  , mapWidth
  , matrix
  ) where
import           HelVM.HelMA.Automata.Piet.Types.Color

import qualified Data.Vector                           as V
import           Lens.Micro.TH

data ColorMap
  = ColorMap
      { _matrix    :: V.Vector (V.Vector Color)
      , _mapWidth  :: Int
      , _mapHeight :: Int
      }
  deriving stock (Eq, Show)

makeLenses ''ColorMap
