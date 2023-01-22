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
divModI  = IAL $ Binaries [Mod, Div]
sInputI  = IAL $ SIO InputChar
sOutputI = IAL $ SIO OutputChar
halibutI = IAL Halibut

dMarkI :: Natural -> Instruction
dMarkI = ICF . DMark

sMarkI :: Natural -> Instruction
sMarkI = ICF . SMark . show

sJumpI :: Natural -> Instruction
sJumpI = ICF . flip CStatic Jump . show

-- | Others

isMark :: Instruction -> Bool
isMark (ICF (DMark _)) = True
isMark (ICF (SMark _)) = True
isMark             _   = False

isDMark :: Natural -> Instruction -> Bool
isDMark n (ICF (DMark n')) = n == n'
isDMark _               _  = False

isSMark :: Label -> Instruction -> Bool
isSMark l (ICF (SMark l')) = l == l'
isSMark _            _     = False

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
