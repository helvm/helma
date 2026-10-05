module HelVM.HelMA.Automaton.LazyLogger where

import           Control.Monad.Logger

import qualified Data.Text.Lazy       as LT

logInfoNL ∷ MonadLogger m ⇒ LT.Text → m ()
logInfoNL = logNL LevelInfo
{-# INLINE logInfoNL #-}

logDebugNL ∷ MonadLogger m ⇒ LT.Text → m ()
logDebugNL = logNL LevelDebug
{-# INLINE logDebugNL #-}

logNL ∷ MonadLogger m ⇒ LogLevel → LT.Text → m ()
logNL l = logWithoutLoc "" l . toLogStr
{-# INLINE logNL #-}
