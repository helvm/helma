{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.Eff.FreeEff (
  interpretFreeEffToMonadEff,
  logInput,
  logOutput,
  FreeEff,
) where

import           HelVM.HelMA.Automaton.API.LogLevel
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
interpretFreeEffFToMonadEff (GetContentsBS    cd) = cd <$> getContentsBS
interpretFreeEffFToMonadEff (GetContentsText  cd) = cd <$> getContentsText
interpretFreeEffFToMonadEff (GetContents      cd) = cd <$> getContents
interpretFreeEffFToMonadEff (GetChar          cd) = cd <$> getChar
interpretFreeEffFToMonadEff (GetLine          cd) = cd <$> eGetLine
interpretFreeEffFToMonadEff (PutChar        c v ) = ePutChar   c $> v
interpretFreeEffFToMonadEff (PutText        s v ) = ePutText   s $> v
interpretFreeEffFToMonadEff (PutTextLn      s v ) = ePutTextLn s $> v
interpretFreeEffFToMonadEff (Flush            v ) = eFlush       $> v
interpretFreeEffFToMonadEff (Log           l m v) = log      l m $> v

----

logInputF :: FreeEffF a -> FreeEff a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ Log Info (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ Log Info           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeEffF a -> FreeEff a
logOutputF f@(PutChar c v)  = liftF (Log Info (one c) v) *> liftF f
logOutputF f@(PutText  s v) = liftF (Log Info       s v) *> liftF f
logOutputF f                =                                liftF f

-- | Instances
instance MonadEff FreeEff where
  getContentsBS   = freeGetContentsBS
  getContentsText = freeGetContentsText
  getContents     = freeGetContents
  getChar         = freeGetChar
  eGetLine         = freeGetLine
  ePutChar         = freePutChar
  ePutText         = freePutText
  ePutTextLn       = freePutTextLn
  eFlush           = freeFlush
  log              = freelog

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

freelog :: LogLevel -> Text -> FreeEff ()
freelog = (liftF .) . flip flip () . Log

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
 | Log              LogLevel Text            a
 deriving stock (Functor)
