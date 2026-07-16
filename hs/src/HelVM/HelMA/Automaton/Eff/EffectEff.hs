{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module HelVM.HelMA.Automaton.Eff.EffectEff (
  EffectEff(..),
) where

import           Effectful

import           HelVM.HelMA.Automaton.API.LogLevel

type instance DispatchOf EffectEff = 'Dynamic

data EffectEff :: Effect where
  GetContentsBS   :: EffectEff m LByteString
  GetContentsText :: EffectEff m LText
  GetChar         :: EffectEff m Char
  GetLine         :: EffectEff m Text
  PutChar         :: Char -> EffectEff m ()
  PutTextEff      :: Text -> EffectEff m ()
  Flush           :: EffectEff m ()
  Log             :: Log -> EffectEff m ()
