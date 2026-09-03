module HelVM.HelMA.Automata.Piet.Types.Matrix
  ( Matrix
  , STMatrix
  ) where

import           Data.Vector         ( Vector )
import           Data.Vector.Mutable ( STVector )


type Matrix a = Vector (Vector a)
type STMatrix s b = Vector (STVector s (Maybe b))
