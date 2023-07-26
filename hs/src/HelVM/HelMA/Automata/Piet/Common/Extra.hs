module HelVM.HelMA.Automata.Piet.Common.Extra where

uncurryComparing :: (a -> Int) -> (a, a) -> Ordering
uncurryComparing = uncurry . comparing

