{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module HelVM.HelMA.Automaton.Eff.AutomatonEff (
  SRAutomatonEff,
  RAutomatonEff,
  SAutomatonEff,
  AutomatonEff,
) where

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Combiner.ALU
import           HelVM.HelMA.Automaton.Combiner.RAM

import           Data.Default                       as Default
import           Data.MonoTraversable               (Element)

type SRAutomatonEff s r m = (SAutomatonEff s m, RAutomatonEff r m, Element s ~ Element r)
type RAutomatonEff  r m   = (RAM r, AutomatonEff (Element r) m)
type SAutomatonEff  s m   = (Stack s, AutomatonEff (Element s) m)
type AutomatonEff   e m   = (ElementConstraint e , AppEff m)

type ElementConstraint e = (ReadShow e , Integral e , Default e)
type ReadShow e          = (Read e , Show e)
