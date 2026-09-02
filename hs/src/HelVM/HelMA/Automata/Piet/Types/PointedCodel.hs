module HelVM.HelMA.Automata.Piet.Types.PointedCodel
  ( PointedCodel (..)
  , makePair
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Codel
import           HelVM.HelMA.Automata.Piet.Types.Cursor

data PointedCodel
  = PointedCodel
      { codel  :: !Codel
      , cursor :: !Cursor
      }
  deriving stock (Eq, Show)

makePair ∷ Cursor → Codel → Maybe PointedCodel
makePair nextCursor codelInfo = Just $ PointedCodel codelInfo nextCursor
