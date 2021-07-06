module HelVM.HelMA.Automaton.IO.BusinessIO (
  SREvaluator,
  REvaluator,
  SEvaluator,
  Evaluator,
  Element,
  BIO,
  BusinessIO,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wPutInt,
  wPutIntegral,
  wLogStr,
  wLogStrLn,
  wLogShow,
) where

import HelVM.Common.Safe

import HelVM.HelMA.Automaton.Memories.RAMConst   as RAM
import HelVM.HelMA.Automaton.Memories.StackConst as Stack

import Data.Default as Default

import qualified System.IO as IO

type SREvaluator e s r m = (Stack e s , RAM e r , Evaluator e m)
type REvaluator e r m = (RAM e r , Evaluator e m)
type SEvaluator e s m = (Stack e s , Evaluator e m)
type Evaluator e m = (Element e , BIO m)

type Element e  = (ReadShow e, Integral e, Default e)
type ReadShow e = (Read e , Show e )
type BIO m = (MonadSafeError m , BusinessIO m)

class Monad m => BusinessIO m where
  wGetChar     :: m Char
  wGetLine     :: m Text
  wPutIntegral :: Integral v => v -> m ()
  wPutInt      :: Int -> m ()
  wPutChar     :: Char -> m ()
  wPutStr      :: Text -> m ()
  wPutStrLn    :: Text -> m ()
  wLogStr      :: Text -> m ()
  wLogStrLn    :: Text -> m ()
  wLogShow     :: Show s => s -> m ()
  wFlush       :: m ()
  wPutIntegral = wPutInt . fromIntegral
  wPutInt      = wPutChar . chr
  wPutStrLn s  = wPutStr $ s <> "\n"
  wLogStrLn s  = wLogStr $ s <> "\n"
  wLogShow     = wLogStrLn . show
  wFlush       = pass

instance BusinessIO IO where
  wGetChar  = IO.getChar
  wGetLine  = getLine
  wPutChar  = IO.putChar
  wPutStr   = putText
  wPutStrLn = putTextLn
  wLogStr   = IO.hPutStr stderr . toString
  wFlush    = hFlush stdout

instance BusinessIO (SafeExceptT IO) where
  wGetChar  = safeExceptT   IO.getChar
  wGetLine  = safeExceptT   getLine
  wPutChar  = safeExceptT . IO.putChar
  wPutStr   = safeExceptT . putText
  wPutStrLn = safeExceptT . putTextLn
  wLogStr   = safeExceptT . IO.hPutStr stderr . toString
  wFlush    = safeExceptT $ hFlush stdout
