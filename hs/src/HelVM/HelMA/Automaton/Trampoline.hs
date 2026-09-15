module HelVM.HelMA.Automaton.Trampoline where

import           Control.Type.Operator

import           Prelude               hiding ( break )

testMaybeLimit ∷ LimitMaybe
testMaybeLimit = Just $ fromIntegral (maxBound :: Int)

trampolineMWithLimit ∷ Monad m ⇒ (a → m $ Same a) → LimitMaybe → a → m a
trampolineMWithLimit f Nothing  !x = trampolineM f x
trampolineMWithLimit f (Just n) !x = trampolineM (actMWithLimit f) (n , x)

actMWithLimit ∷ Monad m ⇒ (a → m $ Same a) → WithLimit a → m $ EitherWithLimit a
actMWithLimit f (!n , !x) = checkN n where
  checkN 0 = pure $ break x
  checkN _ = next n <$> f x

next ∷ Natural → Same a → EitherWithLimit a
next n a = withLimit n <$> a
{-# INLINE next #-}

withLimit ∷ Natural → a → WithLimit a
withLimit !n !a = (n - 1 , a)
{-# INLINE withLimit #-}

trampolineM ∷ Monad m ⇒ (a → m (Either b a)) → a → m b
trampolineM f = fix $ \loop !acc → step loop acc =<< f acc where
  step _    _   (Left b)  = pure b
  step loop _   (Right a) = loop a
{-# INLINE trampolineM #-}

trampoline ∷ (a → Either b a) → a → b
trampoline f = fix $ \loop !acc → step loop (f acc) where
  step _    (Left b)  = b
  step loop (Right a) = loop a
{-# INLINE trampoline #-}

continue ∷ a → Either b a
continue = Right
{-# INLINE continue #-}

break ∷ b → Either b a
break = Left
{-# INLINE break #-}

type LimitMaybe = Maybe Natural

type EitherWithLimit a = Either a $ WithLimit a

type WithLimit a = (Natural , a)

type Same a = Either a a
