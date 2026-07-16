{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module HelVM.HelMA.Automaton.Eff.FusedEff where

import           HelVM.HelMA.Automaton.API.LogLevel

data EffIO m k where
  GetContentsBS   :: EffIO m LByteString
  GetContentsText :: EffIO m LText
  GetChar         :: EffIO m Char
  GetLine         :: EffIO m Text
  PutChar         :: Char -> EffIO m ()
  PutText         :: Text -> EffIO m ()
  PutTextLn       :: Text -> EffIO m ()
  Flush           :: EffIO m ()
  Log             :: Log  -> EffIO m ()
