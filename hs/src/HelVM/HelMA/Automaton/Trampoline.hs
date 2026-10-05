{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automaton.Trampoline where

import           Control.Type.Operator

import qualified Data.Strict.Either    as Strict

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
continueM = pure . Strict.Right
{-# INLINE continueM #-}

breakM ∷ Monad m ⇒ a → SameT m a
breakM = pure . Strict.Left
{-# INLINE breakM #-}

continue ∷ a → Step a
continue = Strict.Right
{-# INLINE continue #-}

break ∷ a → Step a
break = Strict.Left
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
step (Strict.Left !acc)    _ = pure acc
step (Strict.Right !acc) go  = go acc
{-# INLINE step #-}

stepPure ∷ Step a → (a → a) → a
stepPure (Strict.Left !acc)    _ = acc
stepPure (Strict.Right !acc) go  = go acc
{-# INLINE stepPure #-}

-- DATA TYPES AND ALIASES

type LimitMaybe = Maybe Natural
type EitherWithLimit a = Either a $ WithLimit a
type WithLimit a = (Natural , a)
type SameT m a = m $ Same a
type Same a = Step a
type Step a = Strict.Either a a
