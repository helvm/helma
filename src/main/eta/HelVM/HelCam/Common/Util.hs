module HelVM.HelCam.Common.Util where

import Data.Char
import Data.List

type D a = a -> a

type Source = String
type Input  = String
type Output = String

type Interact = Input -> Output

-- ListUtil

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list
  | n > 0 = take n list : chunksOf n (drop n list)
  | otherwise = error "Non positive n"

toList :: a -> [a]
toList = (:[])

appendToList :: [a] -> a -> [a]
appendToList xs x = x : xs

splitBySeparator :: Eq a => a -> [a] -> ([a], [a])
splitBySeparator _ [] = ([], [])
splitBySeparator separator (x:xs)
  | separator == x = ([separator], xs)
  | otherwise = (x:acc, xs') where (acc, xs') = splitBySeparator separator xs

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy separator xs = split $ elemIndex separator xs where
  split Nothing      = (xs, [])
  split (Just index) = (acc, xs') where (acc, _:xs') = splitAt index xs

-- StringUtil

charToString :: Char -> String
charToString = (:[])

splitStringByEndLine :: String -> (String, String)
splitStringByEndLine = splitBy '\n'

toUppers :: Source -> Source
toUppers = map toUpper

-- other

mulAndAdd :: (Integral a) => a -> a -> a -> a
mulAndAdd base digit acc = acc * base + digit

mul2AndAdd :: (Integral a) => a -> a -> a
mul2AndAdd = mulAndAdd 2

mul7AndAdd :: (Integral a) => a -> a -> a
mul7AndAdd = mulAndAdd 7
