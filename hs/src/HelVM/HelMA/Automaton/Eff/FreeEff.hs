{-# LANGUAGE DeriveFunctor #-}
module HelVM.HelMA.Automaton.Eff.FreeEff (
  interpretFreeEffDebug,
  interpretFreeEff,
  logInputFree,
  logOutputFree,
  FreeEff,
  FreeEffF(..),
) where

import qualified HelVM.HelMA.Automaton.API.LogLevel as Legacy
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           Control.Monad.Free
import           Control.Monad.Logger
import           Control.Natural

import           Prelude                            hiding (getLine, putLTextLn, putText, putTextLn)

interpretFreeEffDebug :: (MonadLogger m , MonadEff m)  => FreeEff a -> m a
interpretFreeEffDebug = foldFree interpretFreeEffFDebug

interpretFreeEff :: MonadEff m => FreeEff a -> m a
interpretFreeEff = foldFree interpretFreeEffF

logInputFree :: FreeEff ~> FreeEff
logInputFree = foldFree logInputF

logOutputFree :: FreeEff ~> FreeEff
logOutputFree = foldFree logOutputF

----

interpretFreeEffFDebug :: (MonadLogger m , MonadEff m) => FreeEffF a -> m a
interpretFreeEffFDebug a = logDebugN (name a) *> interpretFreeEffF a

interpretFreeEffF :: MonadEff m => FreeEffF a -> m a
interpretFreeEffF (GetContentsBS    cd) = cd <$> getContentsBS
interpretFreeEffF (GetContentsText  cd) = cd <$> getContentsText
interpretFreeEffF (GetChar          cd) = cd <$> getChar
interpretFreeEffF (GetLine          cd) = cd <$> getLine
interpretFreeEffF (PutChar        c v ) = putChar      c $> v
interpretFreeEffF (PutText        s v ) = putTextEff   s $> v
interpretFreeEffF (Flush            v ) = flush          $> v
interpretFreeEffF (Log            l v ) = log          l $> v

----

logInputF :: FreeEffF a -> FreeEff a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ toLog (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ toLog           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeEffF a -> FreeEff a
logOutputF f@(PutChar c v) = liftF (toLog (one c) v) *> liftF f
logOutputF f@(PutText s v) = liftF (toLog       s v) *> liftF f
logOutputF f               =                            liftF f

toLog :: Text -> a -> FreeEffF a
toLog l = Log (toLogInfo l)

toLogInfo :: Text -> Legacy.Log
toLogInfo = (Legacy.Info, )

-- | Instances
instance MonadEff FreeEff where
  getContentsBS   = freeGetContentsBS
  getContentsText = freeGetContentsText
  getChar         = freeGetChar
  getLine         = freeGetLine
  putChar         = freePutChar
  putTextEff      = freePutTextEff
  flush           = freeFlush
  log             = freelog

-- | Low level functions
freeGetContentsBS :: FreeEff LByteString
freeGetContentsBS = liftF $ GetContentsBS id

freeGetContentsText :: FreeEff LText
freeGetContentsText = liftF $ GetContentsText id

freeGetChar :: FreeEff Char
freeGetChar = liftF $ GetChar id

freeGetLine :: FreeEff Text
freeGetLine = liftF $ GetLine id

freePutChar :: Char -> FreeEff ()
freePutChar = liftF . flip PutChar ()

freePutTextEff :: Text -> FreeEff ()
freePutTextEff = liftF . flip PutText ()

freeFlush :: FreeEff ()
freeFlush = liftF $ Flush ()

freelog :: (Legacy.LogLevel , Text) -> FreeEff ()
freelog = liftF . flip Log ()

name :: FreeEffF a -> Text
name (GetContentsBS   _) = "GetContentsBS"
name (GetContentsText _) = "GetContentsText"
name (GetChar         _) = "GetChar"
name (GetLine         _) = "GetLine"
name (PutChar       _ _) = "PutChar"
name (PutText       _ _) = "PutText"
name (Flush           _) = "Flush"
name (Log           _ _) = "Log"

-- | Types
type FreeEff = Free FreeEffF

data FreeEffF a
 = GetContentsBS             (LByteString -> a)
 | GetContentsText           (LText       -> a)
 | GetChar                   (Char        -> a)
 | GetLine                   (Text        -> a)
 | PutChar          Char                     a
 | PutText          Text                     a
 | Flush                                     a
 | Log              Legacy.Log               a
 deriving stock (Functor)
