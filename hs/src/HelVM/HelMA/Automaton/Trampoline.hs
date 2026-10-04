{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automaton.Trampoline where

import           Control.Type.Operator

import           Prelude               hiding ( break )

-- PUBLIC API

trampolineMWithLimit ∷ Monad m ⇒ (a → SameT m a) → LimitMaybe → a → m a
trampolineMWithLimit f = maybe (loopNoLimit f) (loopWithLimit f . fromIntegral)
{-# INLINE trampolineMWithLimit #-}

trampolineM ∷ Monad m ⇒ (a → SameT m a) → a → m a
trampolineM = loopNoLimit
{-# INLINE trampolineM #-}

trampoline ∷ (a → Same a) → a → a
trampoline f = fix loop where
  loop go !acc = flip stepPure go $ f acc
{-# INLINE trampoline #-}

continueM ∷ Monad m ⇒ a → SameT m a
continueM = pure . Continue
{-# INLINE continueM #-}

breakM ∷ Monad m ⇒ a → SameT m a
breakM = pure . Break
{-# INLINE breakM #-}

continue ∷ a → Step a
continue = Continue
{-# INLINE continue #-}

break ∷ a → Step a
break = Break
{-# INLINE break #-}

testMaybeLimit ∷ LimitMaybe
testMaybeLimit = Just $ fromIntegral (maxBound ∷ Int)

-- PRIVATE OPTIMIZED LOOPS

loopNoLimit ∷ Monad m ⇒ (a → SameT m a) → a → m a
loopNoLimit f = fix loop where
  loop go !acc = flip step go =<< f acc
{-# INLINE loopNoLimit #-}

loopWithLimit ∷ Monad m ⇒ (a → SameT m a) → Word64 → a → m a
loopWithLimit f = fix loop where
  loop go !n !acc | n == 0    = pure acc
                  | otherwise = flip step (go (n - 1)) =<< f acc
{-# INLINE loopWithLimit #-}

step ∷ Monad m ⇒ Step a → (a → m a) → m a
step (Break !acc)    _  = pure acc
step (Continue !acc) go = go acc
{-# INLINE step #-}

stepPure ∷ Step a → (a → a) → a
stepPure (Break !acc)    _  = acc
stepPure (Continue !acc) go = go acc
{-# INLINE stepPure #-}

-- DATA TYPES AND ALIASES

data Step a
  = Break !a
  | Continue !a
  deriving stock (Eq, Read, Show)

type LimitMaybe = Maybe Natural
type EitherWithLimit a = Either a $ WithLimit a
type WithLimit a = (Natural , a)
type SameT m a = m $ Same a
type Same a = Step a
