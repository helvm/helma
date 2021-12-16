module HelVM.HelMA.Automaton.IO.BusinessIO (
  SRLLEvaluator,
  RLLEvaluator,
  SLLEvaluator,
  LLEvaluator,
  SRMTEvaluator,
  RMTEvaluator,
  SMTEvaluator,
  MTEvaluator,
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

import           HelVM.Common.Control.Control
import           HelVM.Common.Control.Safe

import           HelVM.HelMA.Automaton.Memories.LLRAM   as LLRAM
import           HelVM.HelMA.Automaton.Memories.LLStack as LLStack

import           HelVM.HelMA.Automaton.Memories.MTRAM   as MTRAM
import           HelVM.HelMA.Automaton.Memories.MTStack as MTStack

import           Data.Default                           as Default

import qualified System.IO                              as IO

type SRLLEvaluator e s r m = (LLStack.Stack s e, LLRAM.RAM r e, LLEvaluator e m)
type RLLEvaluator e r m = (LLRAM.RAM r e, LLEvaluator e m)
type SLLEvaluator e s m = (LLStack.Stack s e, LLEvaluator e m)
type LLEvaluator e m = (Element e , BIO m)

type SRMTEvaluator s r m = (MTStack.Stack s, MTRAM.RAM r, MTEvaluator m)
type RMTEvaluator r m = (MTRAM.RAM r, MTEvaluator m)
type SMTEvaluator s m = (MTStack.Stack s, MTEvaluator m)
type MTEvaluator m = BIO m

type Element e  = (ReadShow e, Integral e, Default e)
type ReadShow e = (Read e , Show e )
type BIO m = (MonadControl m , BusinessIO m)

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

instance BusinessIO (SafeT IO) where
  wGetChar  = safeT   IO.getChar
  wGetLine  = safeT   getLine
  wPutChar  = safeT . IO.putChar
  wPutStr   = safeT . putText
  wPutStrLn = safeT . putTextLn
  wLogStr   = safeT . IO.hPutStr stderr . toString
  wFlush    = safeT $ hFlush stdout

instance BusinessIO (ControlT IO) where
    wGetChar  = controlT   IO.getChar
    wGetLine  = controlT   getLine
    wPutChar  = controlT . IO.putChar
    wPutStr   = controlT . putText
    wPutStrLn = controlT . putTextLn
    wLogStr   = controlT . IO.hPutStr stderr . toString
    wFlush    = controlT $ hFlush stdout
