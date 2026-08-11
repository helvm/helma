{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.Eff.FreeEff
  ( FreeEff
  , FreeEffF (..)
  , interpretFreeEff
  , interpretFreeEffDebug
  ) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Free.Church          ( F, foldF, liftF )
import           Control.Monad.Logger

import           Prelude                            hiding ( getLine, putLTextLn, putText, putTextLn )
--------------------------------------------------------------------------------

interpretFreeEffDebug ∷ MonadLoggerEff m ⇒ FreeEff a → m a
interpretFreeEffDebug = foldF interpretFreeEffFDebug

interpretFreeEff ∷ MonadEff m ⇒ FreeEff a → m a
interpretFreeEff = foldF interpretFreeEffF

--------------------------------------------------------------------------------

interpretFreeEffFDebug ∷ MonadLoggerEff m ⇒ FreeEffF a → m a
interpretFreeEffFDebug (GetContentsBS    cd) = cd <$> (logDebugN "GetContentsBS"   *> getContentsBS)
interpretFreeEffFDebug (GetContentsText  cd) = cd <$> (logDebugN "GetContentsText" *> getContentsText)
interpretFreeEffFDebug (GetChar          cd) = logAndCont =<< getChar where logAndCont c = logDebugN ("GetChar: " <> one c) $> cd c
interpretFreeEffFDebug (GetLine          cd) = logAndCont =<< getLine where logAndCont l = logDebugN ("GetLine: " <>     l) $> cd l
interpretFreeEffFDebug (PutChar        c v ) = logDebugN ("PutChar: " <> one c) *> putChar    c $> v
interpretFreeEffFDebug (PutText        s v ) = logDebugN ("PutText: " <>     s) *> putLine s $> v
interpretFreeEffFDebug (Flush            v ) = logDebugN "Flush"                *> flush        $> v

interpretFreeEffF ∷ MonadEff m ⇒ FreeEffF a → m a
interpretFreeEffF (GetContentsBS    cd) = cd <$> getContentsBS
interpretFreeEffF (GetContentsText  cd) = cd <$> getContentsText
interpretFreeEffF (GetChar          cd) = cd <$> getChar
interpretFreeEffF (GetLine          cd) = cd <$> getLine
interpretFreeEffF (PutChar        c v ) = putChar      c $> v
interpretFreeEffF (PutText        s v ) = putLine   s $> v
interpretFreeEffF (Flush            v ) = flush          $> v

--------------------------------------------------------------------------------

instance MonadEff FreeEff where
  getContentsBS   = freeGetContentsBS
  getContentsText = freeGetContentsText
  getChar         = freeGetChar
  getLine         = freeGetLine
  putChar         = freePutChar
  putLine         = freePutLine
  flush           = freeFlush

freeGetContentsBS ∷ FreeEff LByteString
freeGetContentsBS = liftF $ GetContentsBS id

freeGetContentsText ∷ FreeEff LText
freeGetContentsText = liftF $ GetContentsText id

freeGetChar ∷ FreeEff Char
freeGetChar = liftF $ GetChar id

freeGetLine ∷ FreeEff Text
freeGetLine = liftF $ GetLine id

freePutChar ∷ Char → FreeEff ()
freePutChar = liftF . flip PutChar ()

freePutLine ∷ Text → FreeEff ()
freePutLine = liftF . flip PutText ()

freeFlush ∷ FreeEff ()
freeFlush = liftF $ Flush ()

--------------------------------------------------------------------------------

type FreeEff = F FreeEffF

data FreeEffF a
  = GetContentsBS (LByteString -> a)
  | GetContentsText (LText -> a)
  | GetChar (Char -> a)
  | GetLine (Text -> a)
  | PutChar Char a
  | PutText Text a
  | Flush a
  deriving stock (Functor)
