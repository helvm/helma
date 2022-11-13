module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction

import           Data.Vector                                     as Vector

-- | Constructors

consI :: Integer -> Instruction
consI = IAL . Cons

subI , divModI , sInputI , sOutputI ,  halibutI :: Instruction
subI     = IAL $ Binary Sub
divModI  = IAL $ Binaries [Sub , Mod]
sInputI  = IAL $ SIO InputChar
sOutputI = IAL $ SIO OutputChar
halibutI = IAL Halibut

markNatI :: Natural -> Instruction
markNatI = ICF . DMark

--transferI :: Instruction
--transferI = ICF Transfer

-- | Others

isMarkNat :: Natural -> Instruction -> Bool
isMarkNat n (ICF (DMark n')) = n == n'
isMarkNat _               _  = False

isMark :: Label -> Instruction -> Bool
isMark l (ICF (SMark l')) = l == l'
isMark _            _     = False

-- | Types

data Instruction =
    IAL !ALInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  | Transfer
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction
