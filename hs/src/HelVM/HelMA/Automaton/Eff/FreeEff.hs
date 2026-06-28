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

interpretFreeEffToMonadEff :: MonadEff m => FreeEff a -> m a
interpretFreeEffToMonadEff = foldFree interpretFreeEffFToMonadEff

logInput :: FreeEff ~> FreeEff
logInput = foldFree logInputF

logOutput :: FreeEff ~> FreeEff
logOutput = foldFree logOutputF

----

interpretFreeEffFToMonadEff :: MonadEff m => FreeEffF a -> m a
interpretFreeEffFToMonadEff (GetContentsBS    cd) = cd <$> eGetContentsBS
interpretFreeEffFToMonadEff (GetContentsText  cd) = cd <$> eGetContentsText
interpretFreeEffFToMonadEff (GetContents      cd) = cd <$> eGetContents
interpretFreeEffFToMonadEff (GetChar          cd) = cd <$> eGetChar
interpretFreeEffFToMonadEff (GetLine          cd) = cd <$> eGetLine
interpretFreeEffFToMonadEff (PutChar        c v ) = ePutChar   c $> v
interpretFreeEffFToMonadEff (PutText        s v ) = ePutText   s $> v
interpretFreeEffFToMonadEff (PutTextLn      s v ) = ePutTextLn s $> v
interpretFreeEffFToMonadEff (LogText        s v ) = eLogText   s $> v
interpretFreeEffFToMonadEff (LogTextLn      s v ) = eLogTextLn s $> v
interpretFreeEffFToMonadEff (Flush            v ) = eFlush       $> v
interpretFreeEffFToMonadEff (ReadFileText   s cd) = cd <$> eReadFileText s

----

logInputF :: FreeEffF a -> FreeEff a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ LogText (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ LogText           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeEffF a -> FreeEff a
logOutputF f@(PutChar c v)  = liftF (LogText (one c) v) *> liftF f
logOutputF f@(PutText  s v) = liftF (LogText       s v) *> liftF f
logOutputF f                =                              liftF f

-- | Instances
instance MonadEff FreeEff where
  eGetContentsBS   = freeGetContentsBS
  eGetContentsText = freeGetContentsText
  eGetContents     = freeGetContents
  eGetChar         = freeGetChar
  eGetLine         = freeGetLine
  ePutChar         = freePutChar
  ePutText         = freePutText
  ePutTextLn       = freePutTextLn
  eLogText         = freeLogText
  eLogTextLn       = freeLogTextLn
  eFlush           = freeFlush
  eReadFileText    = freeReadFileText

instance MonadEff (SafeT FreeEff) where
  eGetContentsBS   = safeT   freeGetContentsBS
  eGetContentsText = safeT   freeGetContentsText
  eGetContents     = safeT   freeGetContents
  eGetChar         = safeT   freeGetChar
  eGetLine         = safeT   freeGetLine
  ePutChar         = safeT . freePutChar
  ePutText         = safeT . freePutText
  ePutTextLn       = safeT . freePutTextLn
  eLogText         = safeT . freeLogText
  eLogTextLn       = safeT . freeLogTextLn
  eFlush           = safeT   freeFlush
  eReadFileText    = safeT . freeReadFileText

instance MonadEff (ControlT FreeEff) where
  eGetContentsBS    = controlT   freeGetContentsBS
  eGetContentsText  = controlT   freeGetContentsText
  eGetContents      = controlT   freeGetContents
  eGetChar          = controlT   freeGetChar
  eGetLine          = controlT   freeGetLine
  ePutChar          = controlT . freePutChar
  ePutText          = controlT . freePutText
  ePutTextLn        = controlT . freePutTextLn
  eLogText          = controlT . freeLogText
  eLogTextLn        = controlT . freeLogTextLn
  eFlush            = controlT   freeFlush
  eReadFileText     = controlT . freeReadFileText

-- | Low level functions
freeGetContentsBS :: FreeEff LByteString
freeGetContentsBS = liftF $ GetContentsBS id

freeGetContentsText :: FreeEff LText
freeGetContentsText = liftF $ GetContentsText id

freeGetContents :: FreeEff String
freeGetContents = liftF $ GetContents id

freeGetChar :: FreeEff Char
freeGetChar = liftF $ GetChar id

freeGetLine :: FreeEff Text
freeGetLine = liftF $ GetLine id

freePutChar :: Char -> FreeEff ()
freePutChar = liftF . flip PutChar ()

freePutText :: Text -> FreeEff ()
freePutText = liftF . flip PutText ()

freePutTextLn :: Text -> FreeEff ()
freePutTextLn = liftF . flip PutTextLn ()

freeLogText :: Text -> FreeEff ()
freeLogText = liftF . flip LogText ()

freeLogTextLn :: Text -> FreeEff ()
freeLogTextLn = liftF . flip LogTextLn ()

freeFlush :: FreeEff ()
freeFlush = liftF $ Flush ()

freeReadFileText :: FilePath -> FreeEff Text
freeReadFileText s = liftF $ ReadFileText s id

-- | Types
type FreeEff = Free FreeEffF

data FreeEffF a
 = GetContentsBS             (LByteString -> a)
 | GetContentsText           (LText       -> a)
 | GetContents               (String      -> a)
 | GetChar                   (Char        -> a)
 | GetLine                   (Text        -> a)
 | PutChar          Char                     a
 | PutText          Text                     a
 | PutTextLn        Text                     a
 | LogText          Text                     a
 | LogTextLn        Text                     a
 | Flush                                     a
 | ReadFileText     FilePath (Text        -> a)
 deriving stock (Functor)
