module HelVM.HelMA.Automata.Piet.Types.Codel
  ( Codel (..)
  , checkColor
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color

data Codel
  = Codel
      { color :: Color
      , index :: Int
      }
  deriving stock (Eq, Show)

checkColor ∷ Codel → Maybe Codel
checkColor codel
  | codel.color == Black = Nothing
  | otherwise            = Just codel
