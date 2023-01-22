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

actMWithLimit :: Monad m => (a -> m $ Same a) -> WithLimit a -> m (Either (WithLimit a) a)
actMWithLimit f (n , x) = checkN n where
  checkN 0 = pure $ break x
  checkN _ = mapLeft (n - 1 , ) <$> f x

break :: b -> Either a b
break = Right

continue :: a -> Either a b
continue = Left

type LimitMaybe = Maybe Natural

type WithLimit a = (Natural , a)

type Same a = Either a a
