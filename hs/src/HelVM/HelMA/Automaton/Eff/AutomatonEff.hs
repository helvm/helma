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

type SRAutomatonEff e s r m = (Stack s e, RAM r e, AutomatonEff e m)
type RAutomatonEff  e r m   = (RAM r e, AutomatonEff e m)
type SAutomatonEff  e s m   = (Stack s e, AutomatonEff e m)
type AutomatonEff   e m     = (Element e , AppEff m)

type Element e  = (ReadShow e , Integral e , Default e)
type ReadShow e = (Read e , Show e)
