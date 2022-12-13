module HelVM.HelMA.Automaton.Units.LSU where

import           HelVM.HelMA.Automaton.Units.ALU
import qualified HelVM.HelMA.Automaton.Units.RAM                 as RAM

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

slInstruction :: (LSU m s r element) => LSInstruction -> LoadStoreUnit s r -> m $ LoadStoreUnit s r
slInstruction Load             = load
slInstruction Store            = store
slInstruction (MIO OutputChar) = loadOutputChar
slInstruction (MIO OutputDec)  = loadOutputDec
slInstruction (MIO InputChar)  = storeInputChar
slInstruction (MIO InputDec)   = storeInputDec

load :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
load (LSU s r) = appendError "LSU.load" $ build <$> pop1 s where
  build (address , s') = LSU (push1 (RAM.genericLoad r address) s') r

store :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
store (LSU s r) = appendError "LSU.store" $ build <$> pop2 s where
  build (value , address , s') = LSU s' $ RAM.store address value r

loadOutputChar :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
loadOutputChar (LSU s r) = appendError "LSU.loadOutputChar" $ build =<< pop1 s where
  build (address , s') = LSU s' r <$ wPutAsChar (RAM.genericLoad r address)

loadOutputDec :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
loadOutputDec (LSU s r) = appendError "LSU.loadOutputDec" $ build =<< pop1 s where
  build (address , s') = LSU s' r <$ wPutAsDec (RAM.genericLoad r address)

storeInputChar :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
storeInputChar (LSU s r) = appendError "LSU.storeInputChar" $ build =<< pop1 s where
  build (address , s') = LSU s' . flip (RAM.store address) r <$> wGetCharAs

storeInputDec :: LSU m s r element => LoadStoreUnit s r -> m $ LoadStoreUnit s r
storeInputDec (LSU s r) = appendError "LSU.storeInputDec" $ build =<< pop1 s where
  build (address , s') = LSU s' . flip (RAM.store address) r <$> wGetDecAs

-- | Types
type LSU m s r element = (ALU m s element , RAM.RAM r element)

data LoadStoreUnit s r = LSU
  { stack :: s
  , ram   :: r
  }
