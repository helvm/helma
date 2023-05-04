module HelVM.HelMA.Automaton.Trampoline where

import           Control.Monad.Extra   hiding (loop)
import           Control.Type.Operator

import           Data.Either.Extra

import           Prelude               hiding (break)

testMaybeLimit :: LimitMaybe
--testMaybeLimit = Just $ fromIntegral (maxBound :: Int)
testMaybeLimit = Just $ fromIntegral (maxBound :: Int32)

trampolineMWithLimit :: Monad m => (a -> m $ Same a) -> LimitMaybe -> a -> m a
trampolineMWithLimit f Nothing  x = trampolineM f x
trampolineMWithLimit f (Just n) x = trampolineM (actMWithLimit f) (n , x)
--trampolineMWithLimit f n x = convert <$> trampolineMWithLimit2 f (n , x) where convert (_ , a) = a

actMWithLimit :: Monad m => (a -> m $ Same a) -> WithLimit a -> m $ EitherWithLimit a
actMWithLimit f (n , x) = checkN n where
  checkN 0 = pure $ break x
  checkN _ = next n <$> f x

trampolineMWithLimit2 :: Monad m => (a -> m $ Same a) -> WithLimitMaybe a -> m $ WithLimitMaybe a
trampolineMWithLimit2 f (Nothing, x) = prod <$> trampolineM f x where prod a = (Nothing , a)
trampolineMWithLimit2 f (Just n , x) = test <$> trampolineM (actMWithLimit2 f) (n , x) where test (n' , a) = (Just (n - n') , a)

actMWithLimit2 :: Monad m => (a -> m $ Same a) -> WithLimit a -> m $ SameWithLimit a
actMWithLimit2 f (n , x) = checkN n where
  checkN 0 = pure $ break (0 , x)
  checkN _ = addContext (n - 1) <$> f x

addContext :: c -> Either a b -> Either (c , a) (c , b)
addContext ctx (Left  a) = Left  (ctx , a)
addContext ctx (Right b) = Right (ctx , b)

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

--type StepM m a = a -> m a
--type Step a = a -> a

type SameWithLimit a = Same (WithLimit a)

type WithLimitMaybe a = (LimitMaybe , a)

type LimitMaybe = Maybe Natural

type EitherWithLimit a = Either a $ WithLimit a

type WithLimit a = (Natural , a)

type Same a = Either a a
