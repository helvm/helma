module HelVM.Common.ZipA where

infixr 9 |><|
(|><|) :: Applicative f => f a1 -> f b1 -> f (a1 , b1)
(|><|) = liftA2 (,)

infixr 9 >><|
(>><|) :: Applicative f => f (a1 , a2) -> f b1 -> f (a1 , a2 , b1)
(>><|) = liftA2 (\(a1 , a2) b1 -> (a1 , a2 , b1))

infixr 9 |><<
(|><<) :: Applicative f => f a1 -> f (b1 , b2) -> f (a1 , b1 , b2)
(|><<) = liftA2 (\a1 (b1 , b2) -> (a1 , b1 , b2))
