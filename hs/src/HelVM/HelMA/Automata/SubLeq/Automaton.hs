module HelVM.HelMA.Automata.SubLeq.Automaton (
  newMemory,
  run,
) where

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Trampoline     as Trampoline

import           HelVM.HelMA.Automaton.Combiner.RAM   as RAM

import           Control.Type.Operator

run :: (RAutomatonIO e r m) => Maybe Natural -> Automaton e r -> m $ Automaton e r
run = trampolineMWithLimit nextState

nextState :: RAutomatonIO e r m => Automaton e r -> m $ AutomatonSame e r
nextState a@(Automaton ic ram)
  | ic  < 0   = doEnd a
  | src < 0   = doInputChar   dst a
  | dst < 0   = doOutputChar  src a
  | otherwise = doInstruction src dst a
    where
      src  = genericLoad ram ic
      dst  = genericLoad ram $ ic + 1

-- | IO instructions
doOutputChar :: RAutomatonIO e r m => e -> Automaton e r -> m $ AutomatonSame e r
doOutputChar address (Automaton ic ram) = wPutAsChar (genericLoad ram address) $> Trampoline.continue (next3Automaton ic ram)

doInputChar :: RAutomatonIO e r m => e -> Automaton e r -> m $ AutomatonSame e r
doInputChar address (Automaton ic ram) = Trampoline.continue . next3Automaton ic . flippedStoreChar address ram <$> wGetChar

-- | Terminate instruction
doEnd :: RAutomatonIO e r m => Automaton e r -> m $ AutomatonSame e r
doEnd = pure . Trampoline.break

doInstruction :: RAutomatonIO e r m => e -> e -> Automaton e r -> m $ AutomatonSame e r
doInstruction src dst (Automaton ic ram) = pure $ Trampoline.continue $ Automaton ic' $ store dst diff ram where
  diff = genericLoad ram dst - genericLoad ram src
  ic'
    | diff <= 0 = genericLoad ram $ ic + 2
    | otherwise = ic + 3

next3Automaton :: Num e => e -> ram -> Automaton e ram
next3Automaton ic = Automaton (ic + 3)

newMemory :: Num e => ram -> Automaton e ram
newMemory = Automaton 0

-- | Types

type AutomatonSame ic ram = Same (Automaton ic ram)

data Automaton ic ram = Automaton
   { memoryIC  :: ic
   , memoryRAM :: ram
   }
  deriving stock (Eq , Read , Show)
