module HelVM.HelMA.Automaton.IO.AutomatonIO (
  SRAutomatonIO,
  RAutomatonIO,
  SAutomatonIO,
  AutomatonIO,
) where

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Units.ALU
import           HelVM.HelMA.Automaton.Units.RAM

type SRAutomatonIO e s r m = (Stack s e, RAM r e, AutomatonIO e m)
type RAutomatonIO  e r m   = (RAM r e, AutomatonIO e m)
type SAutomatonIO  e s m   = (Stack s e, AutomatonIO e m)
type AutomatonIO   e m     = (Element e , BIO m)
