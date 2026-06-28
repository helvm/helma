{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.Eff.FreeEff (
  interpretFreeEffToMonadEff,
  logInput,
  logOutput,
  FreeEff,
) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

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
interpretFreeEffFToMonadEff (Flush            v ) = eFlush       $> v
interpretFreeEffFToMonadEff (ReadFileText   s cd) = cd <$> eReadFileText s
interpretFreeEffFToMonadEff (LogText        s v ) = eLogText   s $> v
interpretFreeEffFToMonadEff (LogTextLn      s v ) = eLogTextLn s $> v

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
  eFlush           = freeFlush
  eReadFileText    = freeReadFileText
  eLogText         = freeLogText
  eLogTextLn       = freeLogTextLn

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

freeFlush :: FreeEff ()
freeFlush = liftF $ Flush ()

freeReadFileText :: FilePath -> FreeEff Text
freeReadFileText s = liftF $ ReadFileText s id

freeLogText :: Text -> FreeEff ()
freeLogText = liftF . flip LogText ()

freeLogTextLn :: Text -> FreeEff ()
freeLogTextLn = liftF . flip LogTextLn ()

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
 | Flush                                     a
 | ReadFileText     FilePath (Text        -> a)
 | LogText          Text                     a
 | LogTextLn        Text                     a
 deriving stock (Functor)
