module HelVM.HelMA.Common.Util where

import Data.Char

type D a = a -> a

type Source = String
type Input  = String
type Output = String

type Interact = Input -> Output

emptyInput :: Input
emptyInput = []

-- ListUtil

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list
  | n > 0 = take n list : chunksOf n (drop n list)
  | otherwise = error "Non positive n"

splitBy :: Eq a => a -> [a] -> ([a] , [a])
splitBy separator xs = (acc , drop 1 xs') where (acc , xs') = break (== separator) xs

-- StringUtil

splitStringByEndLine :: String -> (String , String)
splitStringByEndLine = splitBy '\n'

toUppers :: String -> String
toUppers = map toUpper

-- CharUtil

genericChr :: Integral a => a -> Char
genericChr = chr . fromIntegral

-- other

mulAndAdd :: Integral a => a -> a -> a -> a
mulAndAdd base digit acc = acc * base + digit

mul2AndAdd :: (Integral a) => a -> a -> a
mul2AndAdd = mulAndAdd 2

mul7AndAdd :: (Integral a) => a -> a -> a
mul7AndAdd = mulAndAdd 7
