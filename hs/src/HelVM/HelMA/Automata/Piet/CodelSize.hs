module HelVM.HelMA.Automata.Piet.CodelSize
  ( guessCodelSize
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates

guessCodelSize ∷ Eq a ⇒ Coordinates → (Coordinates → a) → Int
guessCodelSize (width, height) pixelAccessor = groupGCD pixelAccessor (horizontalPositionBlocks width height) `gcd'` groupGCD pixelAccessor (verticalPositionBlocks width height)

horizontalPositionBlocks ∷ Int → Int → [[Coordinates]]
horizontalPositionBlocks width height = generate height (buildRow width)

buildRow ∷ Int → Int → [Coordinates]
buildRow w y = generate w (`makePair` y)

verticalPositionBlocks ∷ Int → Int → [[Coordinates]]
verticalPositionBlocks width height = generate width (buildCol height)

buildCol ∷ Int → Int → [Coordinates]
buildCol h x = generate h (x `makePair`)

makePair ∷ a → b → (a, b)
makePair x y = (x, y)

groupGCD ∷ Eq a ⇒ (i → a) → [[i]] → Int
groupGCD f = fix (groupGCDStep f)

groupGCDStep ∷ Eq a ⇒ (i → a) → ([[i]] → Int) → [[i]] → Int
groupGCDStep _ _ [] = 0
groupGCDStep _ rec ([] : rest) = rec rest
groupGCDStep f rec (positions@(_ : _) : rest) = n `gcd'` rec (nextPositions : rest) where
  (n, nextPositions) = countSameElems f positions

countSameElems ∷ Eq a ⇒ (i → a) → [i] → (Int, [i])
countSameElems _ []         = (0, [])
countSameElems f (pos : xs) = fix (countSameElemsStep f (f pos)) 0 (pos : xs)

countSameElemsStep ∷ Eq a ⇒ (i → a) → a → (Int → [i] → (Int, [i])) → Int → [i] → (Int, [i])
countSameElemsStep _ _ _ acc [] = (acc, [])
countSameElemsStep f firstVal rec acc (x : xs)
  | f x == firstVal = rec (acc + 1) xs
  | otherwise       = (acc, x : xs)

gcd' ∷ Integral a ⇒ a → a → a
gcd' 1 _ = 1
gcd' _ 1 = 1
gcd' a b = gcd a b

generate ∷ Int → (Int → a) → [a]
generate n f = f <$> [0 .. n - 1]
