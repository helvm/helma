module HelVM.HelMA.Automata.Piet.Label (
  LabelKey,
  LabelInfo(..),
  labelSize,
  addPixel,
  LabelBorder(..),
) where

import           HelVM.HelMA.Automata.Piet.Coordinates

type LabelKey = Int

data LabelInfo = LabelInfo
  { _labelSize  :: !Int
  , labelTop    :: !LabelBorder
  , labelLeft   :: !LabelBorder
  , labelBottom :: !LabelBorder
  , labelRight  :: !LabelBorder
  } deriving stock (Show, Eq, Ord)

labelSize :: Maybe LabelInfo -> Int
labelSize Nothing     = 0
labelSize (Just info) = _labelSize info

instance Semigroup LabelInfo where
  s1 <> s2 = LabelInfo
    (_labelSize s1 + _labelSize s2)
    (mergeMin (labelTop s1) (labelTop s2))
    (mergeMin (labelLeft s1) (labelLeft s2))
    (mergeMax (labelBottom s1) (labelBottom s2))
    (mergeMax (labelRight s1) (labelRight s2))

data LabelBorder = LabelBorder
  { borderCoord :: {-# UNPACK #-} !Int
  , borderMin   :: {-# UNPACK #-} !Int
  , borderMax   :: {-# UNPACK #-} !Int
  } deriving stock (Show, Eq, Ord)

mergeMin :: LabelBorder -> LabelBorder -> LabelBorder
mergeMin = merge $ comparing borderCoord

mergeMax :: LabelBorder -> LabelBorder -> LabelBorder
mergeMax = merge $ comparing (negate . borderCoord)

merge :: (LabelBorder -> LabelBorder -> Ordering) -> LabelBorder -> LabelBorder -> LabelBorder
merge comp b1 b2 = go $ comp b1 b2 where
  go EQ = b1 { borderMin = min (borderMin b1) (borderMin b2), borderMax = max (borderMax b1) (borderMax b2) }
  go LT = b1
  go GT = b2

addPixel :: Coordinates -> Maybe LabelInfo -> Maybe LabelInfo
addPixel (x, y) Nothing = Just $ LabelInfo 1 (LabelBorder y x x) (LabelBorder x y y) (LabelBorder y x x) (LabelBorder x y y)
addPixel (x, y) (Just stats) = Just $ stats
  { _labelSize   = 1 + _labelSize stats
  , labelTop    = mergeMin (labelTop stats) (LabelBorder y x x)
  , labelLeft   = mergeMin (labelLeft stats) (LabelBorder x y y)
  , labelBottom = mergeMax (labelBottom stats) (LabelBorder y x x)
  , labelRight  = mergeMax (labelRight stats) (LabelBorder x y y)
  }
