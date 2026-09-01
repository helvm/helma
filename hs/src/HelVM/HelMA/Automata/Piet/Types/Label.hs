module HelVM.HelMA.Automata.Piet.Types.Label
  ( LabelBorder (..)
  , LabelInfo (..)
  , LabelKey
  , addPixel
  , borderCoordL
  , borderMaxL
  , borderMinL
  , getLabelSize
  , labelBottomL
  , labelLeftL
  , labelRightL
  , labelSizeL
  , labelTopL
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Relude.Extra

-- TYPES & LENSES

type LabelKey = Int

data LabelBorder
  = LabelBorder
      { borderCoord :: !Int
      , borderMin   :: !Int
      , borderMax   :: !Int
      }
  deriving stock (Eq, Ord, Show)

-- Lenses dla LabelBorder

borderCoordL ∷ Lens' LabelBorder Int
borderCoordL = lens borderCoord (\s x -> s { borderCoord = x })

borderMinL ∷ Lens' LabelBorder Int
borderMinL = lens borderMin (\s x -> s { borderMin = x })

borderMaxL ∷ Lens' LabelBorder Int
borderMaxL = lens borderMax (\s x -> s { borderMax = x })

data LabelInfo
  = LabelInfo
      { labelSize   :: !Int
      , labelTop    :: !LabelBorder
      , labelLeft   :: !LabelBorder
      , labelBottom :: !LabelBorder
      , labelRight  :: !LabelBorder
      }
  deriving stock (Eq, Ord, Show)

-- Lenses dla LabelInfo

labelSizeL ∷ Lens' LabelInfo Int
labelSizeL = lens labelSize (\s x -> s { labelSize = x })

labelTopL ∷ Lens' LabelInfo LabelBorder
labelTopL = lens labelTop (\s x -> s { labelTop = x })

labelLeftL ∷ Lens' LabelInfo LabelBorder
labelLeftL = lens labelLeft (\s x -> s { labelLeft = x })

labelBottomL ∷ Lens' LabelInfo LabelBorder
labelBottomL = lens labelBottom (\s x -> s { labelBottom = x })

labelRightL ∷ Lens' LabelInfo LabelBorder
labelRightL = lens labelRight (\s x -> s { labelRight = x })

-- EXPORTED FUNCTIONS

addPixel ∷ Coordinates → Maybe LabelInfo → Maybe LabelInfo
addPixel (x, y) Nothing = Just $ LabelInfo 1 (LabelBorder y x x) (LabelBorder x y y) (LabelBorder y x x) (LabelBorder x y y)
addPixel (x, y) (Just stats) = Just $ stats
  & labelSizeL %~ (+ 1)
  & labelTopL %~ (`mergeMin` LabelBorder y x x)
  & labelLeftL %~ (`mergeMin` LabelBorder x y y)
  & labelBottomL %~ (`mergeMax` LabelBorder y x x)
  & labelRightL %~ (`mergeMax` LabelBorder x y y)

getLabelSize ∷ Maybe LabelInfo → Int
getLabelSize Nothing     = 0
getLabelSize (Just info) = info ^. labelSizeL

instance Semigroup LabelInfo where
  s1 <> s2 = LabelInfo
    (s1 ^. labelSizeL + s2 ^. labelSizeL)
    (mergeMin (s1 ^. labelTopL) (s2 ^. labelTopL))
    (mergeMin (s1 ^. labelLeftL) (s2 ^. labelLeftL))
    (mergeMax (s1 ^. labelBottomL) (s2 ^. labelBottomL))
    (mergeMax (s1 ^. labelRightL) (s2 ^. labelRightL))

-- INTERNAL FUNCTIONS

mergeMin ∷ LabelBorder → LabelBorder → LabelBorder
mergeMin = merge $ comparing (^. borderCoordL)

mergeMax ∷ LabelBorder → LabelBorder → LabelBorder
mergeMax = merge $ comparing (negate . (^. borderCoordL))

merge ∷ (LabelBorder → LabelBorder → Ordering) → LabelBorder → LabelBorder → LabelBorder
merge comp b1 b2 = go $ comp b1 b2 where
  go EQ = b1
    & borderMinL %~ min (b2 ^. borderMinL)
    & borderMaxL %~ max (b2 ^. borderMaxL)
  go LT = b1
  go GT = b2
