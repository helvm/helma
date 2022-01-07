module HelVM.HelMA.Automata.False.Util.Util where

subscript :: [a] -> Int -> Maybe a
subscript [] _     = Nothing
subscript (x:_) 0  = Just x
subscript (_:xs) n = subscript xs (n - 1)

charToVar :: Char -> Int
charToVar c = ord c - ord 'a'

charToInteger :: Char -> Integer
charToInteger = fromIntegral . ord

integerToChar :: Integer -> Char
integerToChar = chr . fromInteger
