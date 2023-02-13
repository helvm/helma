module HelVM.HelMA.Automaton.Instruction where

import           HelVM.HelMA.Automaton.Instruction.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.SInstruction

import           Data.Vector                                     as Vector

-- | Constructors

consI :: Integer -> Instruction
consI = sal . Cons

addI , subI , mulI , divI , modI :: Instruction
addI = binary Add
subI = binary Sub
mulI = binary Mul
divI = binary Div
modI = binary Mod

divModI , negI , halibutI :: Instruction
divModI  = binaries [Mod, Div]
negI     = unary Neg
halibutI = sal Halibut

dupI , swapI , rotI , copyTI , discardI :: Instruction
dupI     = copyII 0
swapI    = moveII 1
rotI     = moveII 2
copyTI   = sal $ Indexed Copy TopO
discardI = sal Discard

copyII :: Index -> Instruction
copyII = manipulationII Copy

moveII :: Index -> Instruction
moveII = manipulationII Move

slideII :: Index -> Instruction
slideII = manipulationII Slide

manipulationII :: IndexedOperation -> Index -> Instruction
manipulationII i = sal . Indexed i . ImmediateO

sInputI , sOutputI , sOutputDecI :: Instruction
sInputI     = sio InputChar
sOutputI    = sio OutputChar
sOutputDecI = sio OutputDec

binaries :: [BinaryOperation] -> Instruction
binaries = sal . Binaries

binary :: BinaryOperation -> Instruction
binary = sal . Binary

unary :: UnaryOperation -> Instruction
unary = sal . Unary

sal :: SPureInstruction -> Instruction
sal = IAL . SPure

sio :: IOInstruction -> Instruction
sio = IAL . SIO

markNI :: Natural -> Instruction
markNI = ICF . Mark . MNatural

markSI :: Label -> Instruction
markSI = ICF . Mark . MArtificial

jumpTI :: Instruction
jumpTI = cft Jump

jumpII :: Natural -> Instruction
jumpII = cfi Jump

callSI , jumpSI :: Label -> Instruction
callSI = cfs Call
jumpSI = cfs Jump

bNeTI :: Instruction
bNeTI = cft (Branch NE)

bNeII :: Natural -> Instruction
bNeII = bII NE

bII :: BranchTest ->  Natural -> Instruction
bII t = cfi (Branch t)

bEzSI , bLtzSI :: Label -> Instruction
bEzSI  = cfs (Branch EZ )
bLtzSI = cfs (Branch LTZ)

cft :: LabeledOperation -> Instruction
cft i = ICF $ Labeled i LTop

cfi :: LabeledOperation -> Natural -> Instruction
cfi i = ICF . Labeled i . LImmediate

cfs :: LabeledOperation -> Label -> Instruction
cfs i = ICF . Labeled i . LArtificial

returnI :: Instruction
returnI = ICF Return

storeI , loadI :: Instruction
storeI = ILS Store
loadI  = ILS Load

mInputI , mInputDecI :: Instruction
mInputI    = ILS (MIO InputChar)
mInputDecI = ILS (MIO InputDec )

-- | Others

isICF :: Instruction -> Bool
isICF (ICF _) = True
isICF      _  = False

checkNaturalMark :: Natural -> Instruction -> Bool
checkNaturalMark n (ICF (Mark (MNatural n'))) = n == n'
checkNaturalMark _               _            = False

checkArtificialMark :: Label -> Instruction -> Bool
checkArtificialMark l (ICF (Mark (MArtificial l'))) = l == l'
checkArtificialMark _            _                  = False

-- | Types

data Instruction =
    IAL !SInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction
