module HelVM.HelMA.Automaton.Eff.MonadEff
  ( AppEff
  , MonadEff (..)
  , MonadLoggerEff
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ReadText

import           Control.Monad.Logger

import qualified Data.ByteString.Lazy     as LByteString
import qualified Data.Text.Lazy.IO        as LText

import           Prelude                  hiding (getLine, putText)
import qualified Prelude

import qualified RIO

import qualified System.IO                as IO

type AppEff m = (MonadSafe m , MonadLoggerEff m)

type MonadLoggerEff m = (MonadLogger m, MonadEff m)

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
  getLine         :: m Text
  putChar         :: Char → m ()
  putLine         :: Text → m ()

  flush           :: m ()

  putAsChar      = putIntAsChar . fromIntegral
  putAsDec       = putIntAsDec  . fromIntegral
  getCharAs      = fromIntegral <$> getCharAsInt
  getDecAs       = fromIntegral <$> getDecAsInt

  putIntAsChar   = putChar . chr
  putIntAsDec    = putLine . show
  getCharAsInt   = ord <$> getChar
  getDecAsInt    = readTextUnsafe <$> getLine

  flush          = pass

instance MonadEff IO where
  getContentsBS   = LByteString.getContents
  getContentsText = LText.getContents
  getChar         = IO.getChar
  getLine         = Prelude.getLine
  putChar         = IO.putChar
  putLine         = Prelude.putText
  flush           = flushIO

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) ⇒ MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  getContentsText = lift getContentsText
  getChar         = lift getChar
  getLine         = lift getLine
  putChar         = lift . putChar
  putLine         = lift . putLine
  flush           = lift flush

instance RIO.HasLogFunc env ⇒ MonadEff (RIO.RIO env) where
  getContentsBS   = liftIO LByteString.getContents
  getContentsText = liftIO LText.getContents
  getChar         = liftIO IO.getChar
  getLine         = liftIO Prelude.getLine
  putChar         = liftIO . IO.putChar
  putLine         = liftIO . Prelude.putText
  flush           = liftIO flushIO

---- Internal

flushIO ∷ IO ()
flushIO = hFlush stdout
