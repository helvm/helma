module HelVM.HelMA.Automata.Piet.LLVM.Internal.CodelSize
  ( guessCodelSize
  ) where

guessCodelSize ∷ Eq a ⇒ (Int, Int) → ((Int, Int) → a) → Int
guessCodelSize (width, height) pixelAccessor = codelSize where
  codelSize = groupGCD pixelAccessor horizontalPositionBlocks `gcd'` groupGCD pixelAccessor verticalPositionBlocks
  horizontalPositionBlocks = generate height $ \y -> generate width (,y)
  verticalPositionBlocks = generate width $ \x -> generate height (x,)

generate ∷ Int → (Int → a) → [a]
generate n f = f <$> [0 .. n - 1]

groupGCD ∷ Eq a ⇒ (i → a) → [[i]] → Int
groupGCD _ [] = 0
groupGCD f ([] : rest) = groupGCD f rest
groupGCD f (positions@(pos : _) : rest) = n `gcd'` groupGCD f (nextPositions : rest) where
  (n, nextPositions) = countSameElems 0 positions
  firstVal = f pos
  countSameElems acc [] = (acc, [])
  countSameElems acc (x : xs)
    | f x == firstVal = countSameElems (acc + 1) xs
    | otherwise       = (acc, x : xs)

gcd' ∷ Integral a ⇒ a → a → a
gcd' 1 _ = 1
gcd' _ 1 = 1
gcd' a b = gcd a b
