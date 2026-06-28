{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.Eff.FreeEff (
  interpretFreeEffToMonadEff,
  logInput,
  logOutput,
  FreeEff,
) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Control
import           HelVM.HelIO.Control.Safe

import           Control.Monad.Free
import           Control.Natural

import qualified Data.ByteString.Lazy               as LBS

import qualified Data.Text.Lazy                     as LT

interpretFreeEffToMonadEff :: MonadEff m => FreeEff a -> m a
interpretFreeEffToMonadEff = foldFree interpretFreeEffFToMonadEff

logInput :: FreeEff ~> FreeEff
logInput = foldFree logInputF

logOutput :: FreeEff ~> FreeEff
logOutput = foldFree logOutputF

----

interpretFreeEffFToMonadEff :: MonadEff m => FreeEffF a -> m a
interpretFreeEffFToMonadEff (GetContentsBS   cd) = cd <$> eGetContentsBS
interpretFreeEffFToMonadEff (GetContentsText cd) = cd <$> eGetContentsText
interpretFreeEffFToMonadEff (GetContents     cd) = cd <$> eGetContents
interpretFreeEffFToMonadEff (GetChar         cd) = cd <$> eGetChar
interpretFreeEffFToMonadEff (GetLine         cd) = cd <$> eGetLine
interpretFreeEffFToMonadEff (PutChar        c v) = ePutChar   c $> v
interpretFreeEffFToMonadEff (PutStr         s v) = ePutText   s $> v
interpretFreeEffFToMonadEff (PutStrLn       s v) = ePutTextLn s $> v
interpretFreeEffFToMonadEff (LogStr         s v) = eLogText   s $> v
interpretFreeEffFToMonadEff (LogStrLn       s v) = eLogTextLn s $> v
interpretFreeEffFToMonadEff (Flush            v) = eFlush       $> v

----

logInputF :: FreeEffF a -> FreeEff a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ LogStr (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ LogStr           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeEffF a -> FreeEff a
logOutputF f@(PutChar c v) = liftF (LogStr (one c) v) *> liftF f
logOutputF f@(PutStr  s v) = liftF (LogStr       s v) *> liftF f
logOutputF f               =                             liftF f

-- | Instances
instance MonadEff FreeEff where
  eGetContentsBS   = freeGetContentsBS
  eGetContentsText = freeGetContentsText
  eGetContents     = freeGetContents
  eGetChar         = freeGetChar
  eGetLine         = freeGetLine
  ePutChar         = freePutChar
  ePutText         = freePutStr
  ePutTextLn       = freePutStrLn
  eLogText         = freeLogStr
  eLogTextLn       = freeLogStrLn
  eFlush           = freeFlush

instance MonadEff (SafeT FreeEff) where
  eGetContentsBS   = safeT   freeGetContentsBS
  eGetContentsText = safeT   freeGetContentsText
  eGetContents     = safeT   freeGetContents
  eGetChar         = safeT   freeGetChar
  eGetLine         = safeT   freeGetLine
  ePutChar         = safeT . freePutChar
  ePutText         = safeT . freePutStr
  ePutTextLn       = safeT . freePutStrLn
  eLogText         = safeT . freeLogStr
  eLogTextLn       = safeT . freeLogStrLn
  eFlush           = safeT   freeFlush

instance MonadEff (ControlT FreeEff) where
  eGetContentsBS    = controlT   freeGetContentsBS
  eGetContentsText  = controlT   freeGetContentsText
  eGetContents      = controlT   freeGetContents
  eGetChar          = controlT   freeGetChar
  eGetLine          = controlT   freeGetLine
  ePutChar          = controlT . freePutChar
  ePutText          = controlT . freePutStr
  ePutTextLn        = controlT . freePutStrLn
  eLogText          = controlT . freeLogStr
  eLogTextLn        = controlT . freeLogStrLn
  eFlush            = controlT   freeFlush

-- | Low level functions
freeGetContentsBS :: FreeEff LBS.ByteString
freeGetContentsBS = liftF $ GetContentsBS id

freeGetContentsText :: FreeEff LT.Text
freeGetContentsText = liftF $ GetContentsText id

freeGetContents :: FreeEff String
freeGetContents = liftF $ GetContents id

freeGetChar :: FreeEff Char
freeGetChar = liftF $ GetChar id

freeGetLine :: FreeEff Text
freeGetLine = liftF $ GetLine id

freePutChar :: Char -> FreeEff ()
freePutChar = liftF . flip PutChar ()

freePutStr :: Text -> FreeEff ()
freePutStr = liftF . flip PutStr ()

freePutStrLn :: Text -> FreeEff ()
freePutStrLn = liftF . flip PutStrLn ()

freeLogStr :: Text -> FreeEff ()
freeLogStr = liftF . flip LogStr ()

freeLogStrLn :: Text -> FreeEff ()
freeLogStrLn = liftF . flip LogStrLn ()

freeFlush :: FreeEff ()
freeFlush = liftF $ Flush ()

-- | Types
type FreeEff = Free FreeEffF

data FreeEffF a
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
