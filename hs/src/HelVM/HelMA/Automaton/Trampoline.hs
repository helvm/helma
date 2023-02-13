module HelVM.HelMA.Automaton.Trampoline where

import           Control.Monad.Extra   hiding (loop)
import           Control.Type.Operator

import           Data.Either.Extra

import           Prelude               hiding (break)

testMaybeLimit :: LimitMaybe
testMaybeLimit = Just $ fromIntegral (maxBound :: Int)

trampolineMWithLimit :: Monad m => (a -> m $ Same a) -> LimitMaybe -> a -> m a
trampolineMWithLimit f Nothing  x = trampolineM f x
trampolineMWithLimit f (Just n) x = trampolineM (actMWithLimit f) (n , x)

actMWithLimit :: Monad m => (a -> m $ Same a) -> WithLimit a -> m $ EitherWithLimit a
actMWithLimit f (n , x) = checkN n where
  checkN 0 = pure $ break x
  checkN _ = next n <$> f x

next :: Natural -> Same a -> EitherWithLimit a
next n a = withLimit n <$> a

withLimit :: Natural -> a -> WithLimit a
withLimit n a = (n - 1 , a)

trampolineM :: Monad m => (a -> m (Either b a)) -> a -> m b
trampolineM f = loop where loop = either return loop <=< f

trampoline :: (a -> Either b a) -> a -> b
trampoline f = loop where loop = either id loop . f

continue :: a -> Either b a
continue = Right

break :: b -> Either b a
break = Left

type LimitMaybe = Maybe Natural

type EitherWithLimit a = Either a $ WithLimit a

type WithLimit a = (Natural , a)

type Same a = Either a a
