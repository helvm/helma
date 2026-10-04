module HelVM.HelMA.Automata.Piet.CodelSize
  ( guessCodelSize
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

guessCodelSize ∷ Eq a ⇒ Coordinates → (Coordinates → a) → Int
guessCodelSize (width, height) pixelAccessor = groupGCD (sameRows pixelAccessor width) height `gcd'` groupGCD (sameColumns pixelAccessor height) width

sameRows ∷ Eq a ⇒ (Coordinates → a) → Int → Int → Int → Bool
sameRows pixelAccessor width = samePixels pixelAccessor makePair width

sameColumns ∷ Eq a ⇒ (Coordinates → a) → Int → Int → Int → Bool
sameColumns pixelAccessor height = samePixels pixelAccessor (flip makePair) height

samePixels ∷ Eq a ⇒ (Coordinates → a) → (Int → Int → Coordinates) → Int → Int → Int → Bool
samePixels pixelAccessor makeCoordinates size position1 position2 = all (samePixel pixelAccessor makeCoordinates position1 position2) [0 .. size - 1]

samePixel ∷ Eq a ⇒ (Coordinates → a) → (Int → Int → Coordinates) → Int → Int → Int → Bool
samePixel pixelAccessor makeCoordinates position1 position2 index = pixelAccessor (makeCoordinates index position1) == pixelAccessor (makeCoordinates index position2)

makePair ∷ a → b → (a, b)
makePair x y = (x, y)

groupGCD ∷ (Int → Int → Bool) → Int → Int
groupGCD _     size | size <= 0 = 0
groupGCD equal size             = uncurry gcd' $ foldl' (groupGCDStep equal) (0, 1) [1 .. size - 1]

groupGCDStep ∷ (Int → Int → Bool) → (Int, Int) → Int → (Int, Int)
groupGCDStep _     (1, _) _ = (1, 0)
groupGCDStep equal (g, n) i
  | equal (i - 1) i = (g, n + 1)
  | otherwise       = (gcd' g n, 1)

gcd' ∷ Integral a ⇒ a → a → a
gcd' 1 _ = 1
gcd' _ 1 = 1
gcd' a b = gcd a b
