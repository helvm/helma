module HelVM.HelMA.Automata.Piet.ListExtra where

lastUntil :: Ord a => (a -> Bool) -> [a] -> a
lastUntil _ [x]    = x
lastUntil p (x:xs) = if p x then x else lastUntil p xs
lastUntil _ _      = error "empty list in lastUntil helper (imageGuessCodelLength)"
