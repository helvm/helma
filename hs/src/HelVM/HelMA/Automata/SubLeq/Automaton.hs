{-# LANGUAGE FlexibleContexts #-}
module HelVM.HelMA.Automata.SubLeq.Automaton (
  newMemory,
  runAutomat,
) where

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Trampoline       as Trampoline

import           HelVM.HelMA.Automaton.Combiner.RAM     as RAM

import           Control.Type.Operator

import           Data.MonoTraversable                   (Element)

runAutomat :: RAutomatonEff r m => Maybe Natural -> Automaton r -> m $ Automaton r
runAutomat = trampolineMWithLimit nextState

nextState :: RAutomatonEff r m => Automaton r -> m $ AutomatonSame r
nextState a@(Automaton ic ram)
  | ic  < 0   = doEnd a
  | src < 0   = doInputChar   dst a
  | dst < 0   = doOutputChar  src a
  | otherwise = doInstruction src dst a
    where
      src  = genericLoad ram ic
      dst  = genericLoad ram $ ic + 1

-- | IO instructions
doOutputChar :: RAutomatonEff r m => Element r -> Automaton r -> m $ AutomatonSame r
doOutputChar address (Automaton ic ram) = putAsChar (genericLoad ram address) $> Trampoline.continue (next3Automaton ic ram)

doInputChar :: RAutomatonEff r m => Element r -> Automaton r -> m $ AutomatonSame r
doInputChar address (Automaton ic ram) = Trampoline.continue . next3Automaton ic . flippedStoreChar address ram <$> getChar

-- | Terminate instruction
doEnd :: RAutomatonEff r m => Automaton r -> m $ AutomatonSame r
doEnd = pure . Trampoline.break

doInstruction :: RAutomatonEff r m => Element r -> Element r -> Automaton r -> m $ AutomatonSame r
doInstruction src dst (Automaton ic ram) = pure $ Trampoline.continue $ Automaton ic' $ store dst diff ram where
  diff = genericLoad ram dst - genericLoad ram src
  ic'
    | diff <= 0 = genericLoad ram $ ic + 2
    | otherwise = ic + 3

next3Automaton :: Num (Element r) => Element r -> r -> Automaton r
next3Automaton ic = Automaton (ic + 3)

newMemory :: Num (Element r) => r -> Automaton r
newMemory = Automaton 0

-- | Types

type AutomatonSame r = Same (Automaton r)

data Automaton r = Automaton !(Element r) !r

