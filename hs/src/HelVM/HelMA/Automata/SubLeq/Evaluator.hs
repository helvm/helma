module HelVM.HelMA.Automata.SubLeq.Evaluator (
  doInstruction,
) where

import           HelVM.HelMA.Automaton.IO.BusinessIO
import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Units.RAM      as RAM

import           Control.Type.Operator

doInstruction :: REvaluator e r m => e -> r -> m $ Unit e r
doInstruction ic ram
  | ic  < 0   = doEnd ic ram
  | src < 0   = doInputChar  dst ic ram
  | dst < 0   = doOutputChar src ic ram
  | otherwise = doInstruction ic' $ store dst diff ram
    where
      src  = genericLoad ram ic
      dst  = genericLoad ram $ ic + 1
      diff = genericLoad ram dst - genericLoad ram src
      ic'
        | diff <= 0 = genericLoad ram $ ic + 2
        | otherwise = ic + 3

-- | IO instructions
doOutputChar :: REvaluator e r m => e -> e -> r -> m $ Unit e r
doOutputChar address ic ram = wPutAsChar (genericLoad ram address) *> doInstruction (ic+3) ram

doInputChar :: REvaluator e r m => e -> e -> r -> m $ Unit e r
doInputChar address ic ram = doInputChar' =<< wGetChar where
  doInputChar' char = doInstruction (ic+3) $ storeChar address char ram

-- | Terminate instruction
doEnd :: REvaluator e r m => e -> r -> m $ Unit e r
doEnd ic ram = pure $ Unit ic ram

-- | Types
data Unit ic ram = Unit
   { unitIU  :: ic
   , unitRAM :: ram
   }
  deriving stock (Eq , Read , Show)
