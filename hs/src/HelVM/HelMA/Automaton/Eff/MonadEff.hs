{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelMA.Automaton.Eff.MonadEff (
  runEffIOToMonadEff,
  Element,
  AppEff,
  MonadEff(..),

  logError,
  logWarn,
  logInfo,
  logDebug,
) where

import           HelVM.HelMA.Automaton.Eff.EffectEff

import           HelVM.HelIO.Control.Control
import           HelVM.HelMA.Automaton.API.LogLevel

import           HelVM.HelIO.ReadText

import qualified Data.ByteString.Lazy                as LByteString
import           Data.Default                        as Default
import qualified Data.Text.IO                        as Text
import qualified Data.Text.Lazy.IO                   as LText

import           Effectful
import           Effectful.Dispatch.Dynamic

import           Prelude                             hiding (getLine, putLTextLn, putText, putTextLn)
import qualified Prelude

import qualified RIO

import qualified System.IO                           as IO

runEffIOToMonadEff :: (MonadEff (Eff es)) => Eff (EffectEff : es) a -> Eff es a
runEffIOToMonadEff = interpret $ \_ -> \case
  GetContentsBS   -> getContentsBS
  GetContentsText -> getContentsText
  GetContents     -> getContents
  GetChar         -> getChar
  GetLine         -> getLine
  PutChar c       -> putChar c
  PutTextEff t    -> putTextEff t
  Flush           -> flush
  Log l           -> log l

logError :: MonadEff m => Text -> m ()
logError = logCurry Error

logWarn :: MonadEff m => Text -> m ()
logWarn = logCurry Warn

logInfo :: MonadEff m => Text -> m ()
logInfo = logCurry Info

logDebug :: MonadEff m => Text -> m ()
logDebug = logCurry Debug

logCurry :: MonadEff m => LogLevel -> Text -> m ()
logCurry = curry log

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
type AppEff m = (MonadControl m , MonadEff m)

class Monad m => MonadEff m where

  putAsChar       :: Integral v => v -> m ()
  putAsDec        :: Integral v => v -> m ()
  getCharAs       :: Integral v => m v
  getDecAs        :: Integral v => m v

  putIntAsChar    :: Int -> m ()
  putIntAsDec     :: Int -> m ()
  getCharAsInt    :: m Int
  getDecAsInt     :: m Int

  getContentsBS   :: m LByteString
  getContentsText :: m LText
  getContents     :: m String
  getChar         :: m Char
  getLine         :: m Text
  putChar         :: Char -> m ()
  putTextEff      :: Text -> m ()
  putTextLnEff    :: Text -> m ()
  putLTextLnEff   :: LText -> m ()
  flush           :: m ()

  log             :: Log -> m ()

  putAsChar      = putIntAsChar . fromIntegral
  putAsDec       = putIntAsDec  . fromIntegral
  getCharAs      = fromIntegral <$> getCharAsInt
  getDecAs       = fromIntegral <$> getDecAsInt

  putIntAsChar   = putChar . chr
  putIntAsDec    = putTextEff . show
  getCharAsInt   = ord <$> getChar
  getDecAsInt    = readTextUnsafe <$> getLine

  putTextLnEff s = putTextEff $ s <> "\n"
  putLTextLnEff  = putTextLnEff . toText
  flush          = pass


instance MonadEff IO where
  getContentsBS   = LByteString.getContents
  getContentsText = LText.getContents
  getContents     = IO.getContents
  getChar         = IO.getChar
  getLine         = Prelude.getLine
  putChar         = IO.putChar
  putTextEff      = Prelude.putText
  putTextLnEff    = Prelude.putTextLn
  putLTextLnEff   = Prelude.putLTextLn
  flush           = flushIO
  log             = logIO

instance {-# OVERLAPPABLE #-} (MonadTrans t, Monad m, MonadEff m) => MonadEff (t m) where
  getContentsBS   = lift getContentsBS
  getContentsText = lift getContentsText
  getContents     = lift getContents
  getChar         = lift getChar
  getLine         = lift getLine
  putChar         = lift . putChar
  putTextEff      = lift . putTextEff
  putTextLnEff    = lift . putTextLnEff
  putLTextLnEff   = lift . putLTextLnEff
  flush           = lift flush
  log             = lift . log

instance RIO.HasLogFunc env => MonadEff (RIO.RIO env) where
  getContentsBS   = liftIO LByteString.getContents
  getContentsText = liftIO LText.getContents
  getContents     = liftIO IO.getContents
  getChar         = liftIO IO.getChar
  getLine         = liftIO Prelude.getLine
  putChar         = liftIO . IO.putChar
  putTextEff      = liftIO . Prelude.putText
  putTextLnEff    = liftIO . Prelude.putTextLn
  putLTextLnEff   = liftIO . Prelude.putLTextLn
  flush           = liftIO flushIO
  log             = logRIO

instance EffectEff :> es => MonadEff (Eff es) where
  getContentsBS   = send GetContentsBS
  getContentsText = send GetContentsText
  getContents     = send GetContents
  getChar         = send GetChar
  getLine         = send GetLine
  putChar         = send . PutChar
  putTextEff      = send . PutTextEff
  flush           = send Flush
  log             = send . Log

---- Internal

flushIO :: IO ()
flushIO = hFlush stdout

logIO :: Log -> IO ()
logIO = Text.hPutStrLn stderr . logToTextLn

logRIO :: RIO.HasLogFunc env => Log -> RIO.RIO env ()
logRIO (l , m) = RIO.logGeneric "" (toRioLevel l) $ RIO.display m

toRioLevel :: LogLevel -> RIO.LogLevel
toRioLevel Error = RIO.LevelError
toRioLevel Warn  = RIO.LevelWarn
toRioLevel Info  = RIO.LevelInfo
toRioLevel Debug = RIO.LevelDebug
