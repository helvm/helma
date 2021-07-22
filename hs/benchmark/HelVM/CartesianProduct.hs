module HelVM.CartesianProduct where

infixr 9 |><|
(|><|) :: [a1] -> [b1] -> [(a1 , b1)]
(|><|) = liftA2 (,)

infixr 9 >><|
(>><|) :: [(a1 , a2)] -> [b1] -> [(a1 , a2 , b1)]
(>><|) = liftA2 (\(a1 , a2) b1 -> (a1 , a2 , b1))
