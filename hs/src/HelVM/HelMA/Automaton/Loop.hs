module HelVM.HelMA.Automaton.Loop where

import           Control.Monad.Extra
import           Control.Type.Operator

import           Data.Either.Extra

import           Prelude               hiding (break)

testMaybeLimit :: LimitMaybe
testMaybeLimit = Just $ fromIntegral (maxBound :: Int)

loopMWithLimit :: Monad m => (a -> m $ Same a) -> LimitMaybe -> a -> m a
loopMWithLimit f Nothing  x = loopM f x
loopMWithLimit f (Just n) x = loopM (actMWithLimit f) (n , x)

actMWithLimit :: Monad m => (a -> m $ Same a) -> WithLimit a -> m $ EitherWithLimit a
actMWithLimit f (n , x) = checkN n where
  checkN 0 = pure $ break x
  checkN _ = next n <$> f x

next :: Natural -> Same a -> EitherWithLimit a
next n = mapLeft (n - 1 , )

continue :: a -> Either a b
continue = Left

break :: b -> Either a b
break = Right

type LimitMaybe = Maybe Natural

type EitherWithLimit a = Either (WithLimit a) a

type WithLimit a = (Natural , a)

type Same a = Either a a
