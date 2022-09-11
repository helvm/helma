module HelVM.HelMA.Automaton.IO.BusinessIO (

  Element,
  BIO,
  BusinessIO,

  wPutAsChar,
  wPutAsDec,
  wGetCharAs,
  wGetDecAs,

--  wPutIntAsChar,
--  wPutIntAsDec,
--  wGetCharAsInt,
--  wGetDecAsInt,

  wGetContentsBS,
  wGetContentsText,
  wGetContents,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wLogStr,
  wLogStrLn,
  wLogShow,

  logStr,
  flush,
) where

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.ReadText

import qualified Data.ByteString.Lazy        as LBS
import           Data.Default                as Default
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.IO           as LT

import           System.IO                   hiding (getLine, hFlush, stderr, stdout)

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
type BIO m = (MonadControl m , BusinessIO m)

class Monad m => BusinessIO m where

  wPutAsChar       :: Integral v => v -> m ()
  wPutAsDec        :: Integral v => v -> m ()
  wGetCharAs       :: Integral v => m v
  wGetDecAs        :: Integral v => m v

  wPutIntAsChar    :: Int -> m ()
  wPutIntAsDec     :: Int -> m ()
  wGetCharAsInt    :: m Int
  wGetDecAsInt     :: m Int

  wGetContentsBS   :: m LBS.ByteString
  wGetContentsText :: m LT.Text
  wGetContents     :: m String
  wGetChar         :: m Char
  wGetLine         :: m Text
  wPutChar         :: Char -> m ()
  wPutStr          :: Text -> m ()
  wPutStrLn        :: Text -> m ()
  wLogStr          :: Text -> m ()
  wLogStrLn        :: Text -> m ()
  wLogShow         :: Show s => s -> m ()
  wFlush           :: m ()

  wPutAsChar    = wPutIntAsChar . fromIntegral
  wPutAsDec     = wPutIntAsDec  . fromIntegral
  wGetCharAs    = fromIntegral <$> wGetCharAsInt
  wGetDecAs     = fromIntegral <$> wGetDecAsInt

  wPutIntAsChar = wPutChar . chr
  wPutIntAsDec  = wPutStr . show
  wGetCharAsInt = ord <$> wGetChar
  wGetDecAsInt  = readTextUnsafe <$> wGetLine

  wPutStrLn s   = wPutStr $ s <> "\n"
  wLogStrLn s   = wLogStr $ s <> "\n"
  wLogShow      = wLogStrLn . show
  wFlush        = pass

logStr :: Text -> IO ()
logStr = hPutStrLn stderr . toString

flush :: IO ()
flush = hFlush stdout

instance BusinessIO IO where
  wGetContentsBS   = LBS.getContents
  wGetContentsText = LT.getContents
  wGetContents     = getContents
  wGetChar         = getChar
  wGetLine         = getLine
  wPutChar         = putChar
  wPutStr          = putText
  wPutStrLn        = putTextLn
  wLogStr          = logStr
  wFlush           = flush

type ExceptTLegacy = ExceptT String

exceptTLegacy :: Monad m => m a -> ExceptTLegacy m a
exceptTLegacy a = ExceptT $ pure <$> a

instance BusinessIO (ExceptT String IO) where --FIXXME
  wGetContentsBS   = exceptTLegacy   LBS.getContents
  wGetContentsText = exceptTLegacy   LT.getContents
  wGetContents     = exceptTLegacy   getContents
  wGetChar         = exceptTLegacy   getChar
  wGetLine         = exceptTLegacy   getLine
  wPutChar         = exceptTLegacy . putChar
  wPutStr          = exceptTLegacy . putText
  wPutStrLn        = exceptTLegacy . putTextLn
  wLogStr          = exceptTLegacy . logStr
  wFlush           = exceptTLegacy   flush

instance BusinessIO (SafeT IO) where
  wGetContentsBS   = safeT   LBS.getContents
  wGetContentsText = safeT   LT.getContents
  wGetContents     = safeT   getContents
  wGetChar         = safeT   getChar
  wGetLine         = safeT   getLine
  wPutChar         = safeT . putChar
  wPutStr          = safeT . putText
  wPutStrLn        = safeT . putTextLn
  wLogStr          = safeT . logStr
  wFlush           = safeT   flush

instance BusinessIO (ControlT IO) where
  wGetContentsBS   = controlT   LBS.getContents
  wGetContentsText = controlT   LT.getContents
  wGetContents     = controlT   getContents
  wGetChar         = controlT   getChar
  wGetLine         = controlT   getLine
  wPutChar         = controlT . putChar
  wPutStr          = controlT . putText
  wPutStrLn        = controlT . putTextLn
  wLogStr          = controlT . logStr
  wFlush           = controlT   flush
