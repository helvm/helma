{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module HelVM.HelMA.Automaton.Eff.FusedEff where

import           HelVM.HelMA.Automaton.API.LogLevel

import           Prelude                            hiding (getLine, putLTextLn, putText, putTextLn)

data EffIO m k where
  GetContentsBS   :: EffIO m LByteString
  GetContentsText :: EffIO m LText
  GetContents     :: EffIO m String
  GetChar         :: EffIO m Char
  GetLine         :: EffIO m Text
  PutChar         :: Char -> EffIO m ()
  PutText         :: Text -> EffIO m ()
  PutTextLn       :: Text -> EffIO m ()
  Flush           :: EffIO m ()
  Log             :: Log  -> EffIO m ()
