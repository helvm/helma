module HelVM.HelMA.Automaton.Combiner.LSU where

import           HelVM.HelMA.Automaton.Combiner.ALU
import qualified HelVM.HelMA.Automaton.Combiner.RAM              as RAM

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

runSLI :: (LSU m s r element) => LSInstruction -> LoadStoreMemory s r -> m $ LoadStoreMemory s r
runSLI Load             = load
runSLI Store            = store
runSLI (MIO OutputChar) = loadOutputChar
runSLI (MIO OutputDec)  = loadOutputDec
runSLI (MIO InputChar)  = storeInputChar
runSLI (MIO InputDec)   = storeInputDec

load :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
load (LSM s r) = appendError "LSM.load" $ build <$> pop1 s where
  build (address , s') = LSM (push1 (RAM.genericLoad r address) s') r

store :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
store (LSM s r) = appendError "LSM.store" $ build <$> pop2 s where
  build (value , address , s') = LSM s' $ RAM.store address value r

loadOutputChar :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
loadOutputChar (LSM s r) = appendError "LSM.loadOutputChar" $ build =<< pop1 s where
  build (address , s') = LSM s' r <$ wPutAsChar (RAM.genericLoad r address)

loadOutputDec :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
loadOutputDec (LSM s r) = appendError "LSM.loadOutputDec" $ build =<< pop1 s where
  build (address , s') = LSM s' r <$ wPutAsDec (RAM.genericLoad r address)

storeInputChar :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
storeInputChar (LSM s r) = appendError "LSM.storeInputChar" $ build =<< pop1 s where
  build (address , s') = LSM s' . flip (RAM.store address) r <$> wGetCharAs

storeInputDec :: LSU m s r element => LoadStoreMemory s r -> m $ LoadStoreMemory s r
storeInputDec (LSM s r) = appendError "LSM.storeInputDec" $ build =<< pop1 s where
  build (address , s') = LSM s' . flip (RAM.store address) r <$> wGetDecAs

-- | Types
type LSU m s r element = (ALU m s element , RAM.RAM r element)

data LoadStoreMemory s r = LSM
  { stack :: s
  , ram   :: r
  }
