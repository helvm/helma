{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.IO.FreeIO (
  interpretFreeIOToBusinessIO,
  logInput,
  logOutput,
  FreeIO,
) where

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           Control.Monad.Free
import           Control.Natural

import qualified Data.ByteString.Lazy                as LBS

import qualified Data.Text.Lazy                      as LT

interpretFreeIOToBusinessIO :: BusinessIO m => FreeIO a -> m a
interpretFreeIOToBusinessIO = foldFree interpretFreeIOFToBusinessIO

logInput :: FreeIO ~> FreeIO
logInput = foldFree logInputF

logOutput :: FreeIO ~> FreeIO
logOutput = foldFree logOutputF

----

interpretFreeIOFToBusinessIO :: BusinessIO m => FreeIOF a -> m a
interpretFreeIOFToBusinessIO (GetContentsBS   cd) = cd <$> wGetContentsBS
interpretFreeIOFToBusinessIO (GetContentsText cd) = cd <$> wGetContentsText
interpretFreeIOFToBusinessIO (GetContents     cd) = cd <$> wGetContents
interpretFreeIOFToBusinessIO (GetChar         cd) = cd <$> wGetChar
interpretFreeIOFToBusinessIO (GetLine         cd) = cd <$> wGetLine
interpretFreeIOFToBusinessIO (PutChar        c v) = wPutChar  c $> v
interpretFreeIOFToBusinessIO (PutStr         s v) = wPutStr   s $> v
interpretFreeIOFToBusinessIO (PutStrLn       s v) = wPutStrLn s $> v
interpretFreeIOFToBusinessIO (LogStr         s v) = wLogStr   s $> v
interpretFreeIOFToBusinessIO (LogStrLn       s v) = wLogStrLn s $> v
interpretFreeIOFToBusinessIO (Flush            v) = wFlush      $> v

----

logInputF :: FreeIOF a -> FreeIO a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ LogStr (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ LogStr           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeIOF a -> FreeIO a
logOutputF f@(PutChar c v) = liftF (LogStr (one c) v) *> liftF f
logOutputF f@(PutStr  s v) = liftF (LogStr       s v) *> liftF f
logOutputF f               =                             liftF f

-- | Instances
instance BusinessIO FreeIO where
  wGetContentsBS   = freeGetContentsBS
  wGetContentsText = freeGetContentsText
  wGetContents     = freeGetContents
  wGetChar         = freeGetChar
  wGetLine         = freeGetLine
  wPutChar         = freePutChar
  wPutStr          = freePutStr
  wPutStrLn        = freePutStrLn
  wLogStr          = freeLogStr
  wLogStrLn        = freeLogStrLn
  wFlush           = freeFlush

instance BusinessIO (SafeT FreeIO) where
  wGetContentsBS   = safeT   freeGetContentsBS
  wGetContentsText = safeT   freeGetContentsText
  wGetContents     = safeT   freeGetContents
  wGetChar         = safeT   freeGetChar
  wGetLine         = safeT   freeGetLine
  wPutChar         = safeT . freePutChar
  wPutStr          = safeT . freePutStr
  wPutStrLn        = safeT . freePutStrLn
  wLogStr          = safeT . freeLogStr
  wLogStrLn        = safeT . freeLogStrLn
  wFlush           = safeT   freeFlush

instance BusinessIO (ControlT FreeIO) where
  wGetContentsBS    = controlT   freeGetContentsBS
  wGetContentsText  = controlT   freeGetContentsText
  wGetContents      = controlT   freeGetContents
  wGetChar          = controlT   freeGetChar
  wGetLine          = controlT   freeGetLine
  wPutChar          = controlT . freePutChar
  wPutStr           = controlT . freePutStr
  wPutStrLn         = controlT . freePutStrLn
  wLogStr           = controlT . freeLogStr
  wLogStrLn         = controlT . freeLogStrLn
  wFlush            = controlT   freeFlush

-- | Low level functions
freeGetContentsBS :: FreeIO LBS.ByteString
freeGetContentsBS = liftF $ GetContentsBS id

freeGetContentsText :: FreeIO LT.Text
freeGetContentsText = liftF $ GetContentsText id

freeGetContents :: FreeIO String
freeGetContents = liftF $ GetContents id

freeGetChar :: FreeIO Char
freeGetChar = liftF $ GetChar id

freeGetLine :: FreeIO Text
freeGetLine = liftF $ GetLine id

freePutChar :: Char -> FreeIO ()
freePutChar = liftF . flip PutChar ()

freePutStr :: Text -> FreeIO ()
freePutStr = liftF . flip PutStr ()

freePutStrLn :: Text -> FreeIO ()
freePutStrLn = liftF . flip PutStrLn ()

freeLogStr :: Text -> FreeIO ()
freeLogStr = liftF . flip LogStr ()

freeLogStrLn :: Text -> FreeIO ()
freeLogStrLn = liftF . flip LogStrLn ()

freeFlush :: FreeIO ()
freeFlush = liftF $ Flush ()

-- | Types
type FreeIO = Free FreeIOF

data FreeIOF a
 = GetContentsBS   (LBS.ByteString -> a)
 | GetContentsText (LT.Text        -> a)
 | GetContents     (String         -> a)
 | GetChar         (Char           -> a)
 | GetLine         (Text           -> a)
 | PutChar          Char a
 | PutStr           Text a
 | PutStrLn         Text a
 | LogStr           Text a
 | LogStrLn         Text a
 | Flush                 a
 deriving stock (Functor)
