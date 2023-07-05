module HelVM.HelMA.Automata.Piet.FullInfo where

import           HelVM.HelMA.Automata.Piet.LabelBorder

type LabelInfo = Maybe FullInfo

data FullInfo = FullInfo
    { labelSize   :: !Int
    , labelTop    :: LabelBorder
    , labelLeft   :: LabelBorder
    , labelBottom :: LabelBorder
    , labelRight  :: LabelBorder
    }
  deriving stock (Show, Eq, Ord)

instance Semigroup FullInfo where
  (<>) i1 i2 = concatInfo i1 i2

concatInfo :: FullInfo -> FullInfo -> FullInfo
concatInfo i1 i2 = FullInfo
  { labelSize = labelSize i1 + labelSize i2
  , labelTop    = mergeMin (labelTop    i1 , labelTop    i2)
  , labelLeft   = mergeMin (labelLeft   i1 , labelLeft   i2)
  , labelBottom = mergeMax (labelBottom i1 , labelBottom i2)
  , labelRight  = mergeMax (labelRight  i1 , labelRight  i2)
  }
