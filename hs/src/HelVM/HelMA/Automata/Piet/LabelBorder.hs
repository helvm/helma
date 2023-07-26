module HelVM.HelMA.Automata.Piet.LabelBorder where

import           HelVM.HelMA.Automata.Piet.Common.Extra

mergeMin :: LabelBorder2 -> LabelBorder
mergeMin = merge (uncurryComparing borderCoord)

mergeMax :: LabelBorder2 -> LabelBorder
mergeMax = merge (uncurryComparing (negate . borderCoord))

merge :: (LabelBorder2 -> Ordering) -> LabelBorder2 -> LabelBorder
merge comp = flip mergeWithOrdering <*> comp

mergeWithOrdering :: Ordering -> LabelBorder2 -> LabelBorder
mergeWithOrdering EQ = mergeEQ
mergeWithOrdering LT = fst
mergeWithOrdering GT = snd

mergeEQ :: LabelBorder2 -> LabelBorder
mergeEQ (b1 , b2) =  b1
  { borderMin  = min (borderMin b1) (borderMin b2)
  , borderMax  = max (borderMax b1) (borderMax b2)
  }

type LabelBorder2 = (LabelBorder , LabelBorder)

data LabelBorder = LabelBorder
  { borderCoord :: !Int
  , borderMin   ::  !Int
  , borderMax   ::  !Int
  } deriving stock (Show, Eq, Ord)
