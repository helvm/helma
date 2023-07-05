module HelVM.HelMA.Automata.Piet.Extra where

uncurryComparing :: (a -> Int) -> (a, a) -> Ordering
uncurryComparing = uncurry . comparing
