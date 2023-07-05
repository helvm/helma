module HelVM.HelMA.Automata.Piet.Pair where

import           Data.Tuple.Extra

newtype Pair a  = Pair (a , a)

instance Functor Pair where
  fmap f (Pair a) = Pair $ both f a
