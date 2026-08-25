module HelVM.HelMA.Automata.Piet.LLVM.Internal.CodelSize
  ( guessCodelSize
  ) where

-- | Guess the codel size of a given image.
guessCodelSize ∷ Eq a ⇒ (Int, Int) → ((Int, Int) → a) → Int
guessCodelSize (width, height) pixelAccessor = codelSize where
  codelSize = groupGCD pixelAccessor horizontalPositionBlocks `gcd'` groupGCD pixelAccessor verticalPositionBlocks
  horizontalPositionBlocks = generate height $ \y -> generate width $ \x -> (x, y)
  verticalPositionBlocks = generate width $ \x -> generate height $ \y -> (x, y)

generate ∷ Int → (Int → a) → [a]
generate n f = f <$> [0 .. n - 1]

groupGCD ∷ Eq a ⇒ (i → a) → [[i]] → Int
groupGCD _ [] = 0
groupGCD f ([] : rest) = groupGCD f rest
groupGCD f (positions : rest) = n `gcd'` groupGCD f (nextPositions : rest) where
  (n, nextPositions) = countSameElems 0 positions
  first = f $ head positions
  countSameElems acc []       = (acc, [])
  countSameElems acc (x : xs) = if f x == first then countSameElems (acc + 1) xs else (acc, x : xs)

gcd' ∷ Integral a ⇒ a → a → a
gcd' 1 _ = 1
gcd' _ 1 = 1
gcd' a b = gcd a b
