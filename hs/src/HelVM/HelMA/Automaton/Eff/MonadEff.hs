module HelVM.HelMA.Automaton.Eff.MonadEff
  ( AppEff
  , AppSafeEff
  , MonadEff (..)
  ) where

import           HelVM.HelMA.Automaton.API.Env

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ReadText

import           Control.Monad.Logger

import qualified RIO

type AppSafeEff m = (MonadSafe m , AppEff m)

type AppEff m = (MonadLogger m, MonadEff m)

class Monad m ⇒ MonadEff m where

  putAsChar       ∷ Integral v ⇒ v → m ()
  putAsDec        ∷ Integral v ⇒ v → m ()
  getCharAs       ∷ Integral v ⇒ m v
  getDecAs        ∷ Integral v ⇒ m v

  putIntAsChar    ∷ Int → m ()
  putIntAsDec     ∷ Int → m ()
  getCharAsInt    ∷ m Int
  getDecAsInt     ∷ m Int

  getContentsBS   ∷ m LByteString
  getContentsText ∷ m LText
  getChar         ∷ m Char
  getChars        ∷ m Text
  putChar         ∷ Char → m ()
  putChars        ∷ Text → m ()

  flush           ∷ m ()

  putAsChar      = putIntAsChar . fromIntegral
  {-# INLINE putAsChar #-}

  putAsDec       = putIntAsDec  . fromIntegral
  {-# INLINE putAsDec #-}

  getCharAs      = fromIntegral <$> getCharAsInt
  {-# INLINE getCharAs #-}

  getDecAs       = fromIntegral <$> getDecAsInt
  {-# INLINE getDecAs #-}

  putIntAsChar   = putChar . chr
  {-# INLINE putIntAsChar #-}

  putIntAsDec    = putChars . show
  {-# INLINE putIntAsDec #-}

  getCharAsInt   = ord <$> getChar
  {-# INLINE getCharAsInt #-}

  getDecAsInt    = readTextUnsafe <$> getChars
  {-# INLINE getDecAsInt #-}

  flush          = pass
  {-# INLINE flush #-}

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) ⇒ MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  {-# INLINE getContentsBS #-}

  getContentsText = lift getContentsText
  {-# INLINE getContentsText #-}

  getChar         = lift getChar
  {-# INLINE getChar #-}

  getChars        = lift getChars
  {-# INLINE getChars #-}

  putChar         = lift . putChar
  {-# INLINE putChar #-}

  putChars        = lift . putChars
  {-# INLINE putChars #-}

  flush           = lift flush
  {-# INLINE flush #-}

instance {-# OVERLAPPING #-} MonadEff (RIO.RIO Env) where
  getContentsBS   = getContentsBSRio
  {-# INLINE getContentsBS #-}

  getContentsText = getContentsTextRio
  {-# INLINE getContentsText #-}

  getChar         = getCharRio
  {-# INLINE getChar #-}

  getChars        = getCharsRio
  {-# INLINE getChars #-}

  putChar         = putCharRio
  {-# INLINE putChar #-}

  putChars        = putCharsRio
  {-# INLINE putChars #-}

  flush           = pass
  {-# INLINE flush #-}
