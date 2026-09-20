module HelVM.HelMA.Automaton.Eff.MonadEff
  ( AppEff
  , AppSafeEff
  , MonadEff (..)
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ReadText

import           Control.Monad.Logger

import qualified Data.ByteString.Lazy          as LByteString
import qualified Data.Text.Lazy.IO             as LText

import qualified HelVM.HelMA.Automaton.API.Env as Env

import qualified RIO

import qualified System.IO                     as IO

type AppSafeEff m = (MonadSafe m , AppEff m)

type AppEff m = (MonadLogger m, MonadEff m)

class Monad m => MonadEff m where

  putAsChar       :: Integral v ⇒ v → m ()
  putAsDec        :: Integral v ⇒ v → m ()
  getCharAs       :: Integral v ⇒ m v
  getDecAs        :: Integral v ⇒ m v

  putIntAsChar    :: Int → m ()
  putIntAsDec     :: Int → m ()
  getCharAsInt    :: m Int
  getDecAsInt     :: m Int

  getContentsBS   :: m LByteString
  getContentsText :: m LText
  getChar         :: m Char
  getChars        :: m Text
  putChar         :: Char → m ()
  putChars        :: Text → m ()

  flush           :: m ()

  putAsChar      = putIntAsChar . fromIntegral
  putAsDec       = putIntAsDec  . fromIntegral
  getCharAs      = fromIntegral <$> getCharAsInt
  getDecAs       = fromIntegral <$> getDecAsInt

  putIntAsChar   = putChar . chr
  putIntAsDec    = putChars . show
  getCharAsInt   = ord <$> getChar
  getDecAsInt    = readTextUnsafe <$> getChars

  flush          = pass

instance MonadEff IO where
  getContentsBS   = LByteString.getContents
  getContentsText = LText.getContents
  getChar         = IO.getChar
  getChars        = getLine
  putChar         = IO.putChar
  putChars        = putText
  flush           = flushIO

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) ⇒ MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  getContentsText = lift getContentsText
  getChar         = lift getChar
  getChars        = lift getChars
  putChar         = lift . putChar
  putChars        = lift . putChars
  flush           = lift flush

instance {-# OVERLAPPING #-} MonadEff (RIO.RIO Env.Env) where
  getContentsBS   = Env.getContentsBSRio
  getContentsText = Env.getContentsTextRio
  getChar         = Env.getCharRio
  getChars        = Env.getCharsRio
  putChar         = Env.putCharRio
  putChars        = Env.putCharsRio
  flush           = pass

---- Internal

flushIO ∷ IO ()
flushIO = hFlush stdout
