module HelVM.HelCam.Common.Util where

import Data.List
import Text.Read

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

-- StringUtil

charToString :: Char -> String
charToString = (:[])

splitStringByEndLine :: String -> (String, String)
splitStringByEndLine = splitBy '\n'

splitBySeparator :: Eq a => a -> [a] -> ([a], [a])
splitBySeparator _ [] = ([], [])
splitBySeparator separator (x:xs)
  | separator == x = ([separator], xs)
  | otherwise = (x:acc, xs') where (acc, xs') = splitBySeparator separator xs

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy separator xs = split $ elemIndex separator xs where
  split Nothing      = (xs, [])
  split (Just index) = (acc, xs') where (acc, (_:xs')) = splitAt index xs

readOrError :: Read a => String -> a
readOrError raw = match $ readEither raw where
  match (Right result) = result
  match (Left message) = error $ message ++ " [" ++ raw ++ "]"
