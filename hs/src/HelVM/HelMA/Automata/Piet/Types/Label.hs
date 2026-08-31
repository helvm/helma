module HelVM.HelMA.Automata.Piet.Types.Label
  ( LabelBorder (..)
  , LabelInfo (..)
  , LabelKey
  , addPixel
  , borderCoord
  , borderMax
  , borderMin
  , getLabelSize
  , labelBottom
  , labelLeft
  , labelRight
  , labelSize
  , labelTop
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

import           Relude.Extra

-- TYPES & LENSES

type LabelKey = Int

data LabelBorder
  = LabelBorder
      { _borderCoord :: !Int
      , _borderMin   :: !Int
      , _borderMax   :: !Int
      }
  deriving stock (Eq, Ord, Show)

-- Lenses dla LabelBorder

borderCoord ∷ Lens' LabelBorder Int
borderCoord = lens _borderCoord (\s x -> s { _borderCoord = x })

borderMin ∷ Lens' LabelBorder Int
borderMin = lens _borderMin (\s x -> s { _borderMin = x })

borderMax ∷ Lens' LabelBorder Int
borderMax = lens _borderMax (\s x -> s { _borderMax = x })

data LabelInfo
  = LabelInfo
      { _labelSize   :: !Int
      , _labelTop    :: !LabelBorder
      , _labelLeft   :: !LabelBorder
      , _labelBottom :: !LabelBorder
      , _labelRight  :: !LabelBorder
      }
  deriving stock (Eq, Ord, Show)

-- Lenses dla LabelInfo

labelSize ∷ Lens' LabelInfo Int
labelSize = lens _labelSize (\s x -> s { _labelSize = x })

labelTop ∷ Lens' LabelInfo LabelBorder
labelTop = lens _labelTop (\s x -> s { _labelTop = x })

labelLeft ∷ Lens' LabelInfo LabelBorder
labelLeft = lens _labelLeft (\s x -> s { _labelLeft = x })

labelBottom ∷ Lens' LabelInfo LabelBorder
labelBottom = lens _labelBottom (\s x -> s { _labelBottom = x })

labelRight ∷ Lens' LabelInfo LabelBorder
labelRight = lens _labelRight (\s x -> s { _labelRight = x })

-- EXPORTED FUNCTIONS

addPixel ∷ Coordinates → Maybe LabelInfo → Maybe LabelInfo
addPixel (x, y) Nothing = Just $ LabelInfo 1 (LabelBorder y x x) (LabelBorder x y y) (LabelBorder y x x) (LabelBorder x y y)
addPixel (x, y) (Just stats) = Just $ stats
  & labelSize %~ (+ 1)
  & labelTop %~ (`mergeMin` LabelBorder y x x)
  & labelLeft %~ (`mergeMin` LabelBorder x y y)
  & labelBottom %~ (`mergeMax` LabelBorder y x x)
  & labelRight %~ (`mergeMax` LabelBorder x y y)

getLabelSize ∷ Maybe LabelInfo → Int
getLabelSize Nothing     = 0
getLabelSize (Just info) = info ^. labelSize

instance Semigroup LabelInfo where
  s1 <> s2 = LabelInfo
    (s1 ^. labelSize + s2 ^. labelSize)
    (mergeMin (s1 ^. labelTop) (s2 ^. labelTop))
    (mergeMin (s1 ^. labelLeft) (s2 ^. labelLeft))
    (mergeMax (s1 ^. labelBottom) (s2 ^. labelBottom))
    (mergeMax (s1 ^. labelRight) (s2 ^. labelRight))

-- INTERNAL FUNCTIONS

mergeMin ∷ LabelBorder → LabelBorder → LabelBorder
mergeMin = merge $ comparing (^. borderCoord)

mergeMax ∷ LabelBorder → LabelBorder → LabelBorder
mergeMax = merge $ comparing (negate . (^. borderCoord))

merge ∷ (LabelBorder → LabelBorder → Ordering) → LabelBorder → LabelBorder → LabelBorder
merge comp b1 b2 = go $ comp b1 b2 where
  go EQ = b1
    & borderMin %~ min (b2 ^. borderMin)
    & borderMax %~ max (b2 ^. borderMax)
  go LT = b1
  go GT = b2
