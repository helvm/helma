{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automaton.Trampoline where

import           Control.Type.Operator

import           Prelude               hiding ( break )

-- PUBLIC API

trampolineMWithLimit ∷ Monad m ⇒ (a → SameT m a) → LimitMaybe → a → m a
trampolineMWithLimit f Nothing   !x = loopNoLimit f x
trampolineMWithLimit f (Just !n) !x = loopWithLimit f (fromIntegral n) x
{-# INLINE trampolineMWithLimit #-}

-- PRIVATE OPTIMIZED LOOPS

loopNoLimit ∷ Monad m ⇒ (a → SameT m a) → a → m a
loopNoLimit f !acc = f acc >>= either pure (loopNoLimit f)
{-# INLINE loopNoLimit #-}

loopWithLimit ∷ Monad m ⇒ (a → SameT m a) → Word64 → a → m a
loopWithLimit _ 0  !acc = pure acc
loopWithLimit f !n !acc = f acc >>= either pure (loopWithLimit f (n - 1))
{-# INLINE loopWithLimit #-}

-- UTILITIES / LEGACY HELPERS (for compatibility)

testMaybeLimit ∷ LimitMaybe
testMaybeLimit = Just $ fromIntegral (maxBound ∷ Int)

trampolineM ∷ Monad m ⇒ (a → SameT m a) → a → m a
trampolineM f !acc = f acc >>= either pure (trampolineM f)
{-# INLINE trampolineM #-}

trampoline ∷ (a → Same a) → a → a
trampoline f !acc = either id (trampoline f) (f acc)
{-# INLINE trampoline #-}

continueM ∷ Monad m ⇒ a → SameT m a
continueM  = pure . Right
{-# INLINE continueM #-}

breakM ∷ Monad m ⇒ a → SameT m a
breakM = pure . Left
{-# INLINE breakM #-}


type LimitMaybe = Maybe Natural
type EitherWithLimit a = Either a $ WithLimit a
type WithLimit a = (Natural , a)
type SameT m a = m $ Same a
type Same a = Either a a
