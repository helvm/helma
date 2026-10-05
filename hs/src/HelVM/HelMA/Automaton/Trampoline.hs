{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automaton.Trampoline
  ( LimitMaybe
  , SameT
  , break
  , breakM
  , continue
  , continueM
  , testMaybeLimit
  , trampoline
  , trampolineM
  , trampolineMWithLimit
  ) where

-- import           Control.Monad.Loop
import           Control.Type.Operator
import qualified Data.Strict.Either    as Strict
import           Prelude               hiding ( break )

-- PUBLIC API

trampolineMWithLimit ∷ Monad m ⇒ (a → SameT m a) → LimitMaybe → a → m a
trampolineMWithLimit f = maybe (trampolineM f) (loopWithLimit f . fromIntegral)
{-# INLINE trampolineMWithLimit #-}

trampolineM ∷ Monad m ⇒ (a → SameT m a) → a → m a
trampolineM f = fix loop where
  loop go !acc = stepM go =<< f acc
{-# INLINE trampolineM #-}

trampoline ∷ (a → Same a) → a → a
trampoline f = fix loop where
  loop go !acc = stepPure (f acc) go
{-# INLINE trampoline #-}

continueM ∷ Monad m ⇒ a → SameT m a
continueM = pure . continue
{-# INLINE continueM #-}

breakM ∷ Monad m ⇒ a → SameT m a
breakM = pure . break
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

loopWithLimit ∷ Monad m ⇒ (a → SameT m a) → Word64 → a → m a
loopWithLimit f = fix loop where
  loop go !n !acc | n == 0    = pure acc
                  | otherwise = stepM (go (n - 1)) =<< f acc
{-# INLINE loopWithLimit #-}

stepM ∷ Monad m ⇒ (a → m a) → Step a → m a
stepM _  (Strict.Left !acc)  = pure acc
stepM go (Strict.Right !acc) = go acc
{-# INLINE stepM #-}

stepPure ∷ Step a → (a → a) → a
stepPure (Strict.Left !acc)  _  = acc
stepPure (Strict.Right !acc) go = go acc
{-# INLINE stepPure #-}


-- DATA TYPES AND ALIASES

type LimitMaybe = Maybe Natural
type SameT m a = m $ Same a
type Same a = Step a
type Step a = Strict.Either a a
