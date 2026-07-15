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

import           Prelude                            hiding (getLine, putLTextLn, putText, putTextLn)

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
interpretFreeEffFToMonadEff (GetLine          cd) = cd <$> getLine
interpretFreeEffFToMonadEff (PutChar        c v ) = putChar      c $> v
interpretFreeEffFToMonadEff (PutText        s v ) = putTextEff   s $> v
interpretFreeEffFToMonadEff (PutTextLn      s v ) = putTextLnEff s $> v
interpretFreeEffFToMonadEff (Flush            v ) = flush          $> v
interpretFreeEffFToMonadEff (Log            l v ) = log          l $> v

----

logInputF :: FreeEffF a -> FreeEff a
logInputF (GetChar     cd) = freeGetChar     >>= (\c -> liftF $ toLog (one      c) (cd c))
logInputF (GetLine     cd) = freeGetLine     >>= (\l -> liftF $ toLog           l  (cd l))
logInputF               f  =                            liftF f

logOutputF :: FreeEffF a -> FreeEff a
logOutputF f@(PutChar c v)  = liftF (toLog (one c) v) *> liftF f
logOutputF f@(PutText  s v) = liftF (toLog       s v) *> liftF f
logOutputF f                =                            liftF f

toLog :: Text -> a -> FreeEffF a
toLog l = Log (toLogInfo l)

toLogInfo :: Text -> Log
toLogInfo = (Info, )

-- | Instances
instance MonadEff FreeEff where
  getContentsBS   = freeGetContentsBS
  getContentsText = freeGetContentsText
  getContents     = freeGetContents
  getChar         = freeGetChar
  getLine         = freeGetLine
  putChar         = freePutChar
  putTextEff      = freputTextEff
  putTextLnEff    = freputTextLnEff
  flush           = freeFlush
  log             = freelog

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

freputTextEff :: Text -> FreeEff ()
freputTextEff = liftF . flip PutText ()

freputTextLnEff :: Text -> FreeEff ()
freputTextLnEff = liftF . flip PutTextLn ()

freeFlush :: FreeEff ()
freeFlush = liftF $ Flush ()

freelog :: (LogLevel , Text) -> FreeEff ()
freelog = liftF . flip Log ()

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
 | Log              Log                      a
 deriving stock (Functor)
