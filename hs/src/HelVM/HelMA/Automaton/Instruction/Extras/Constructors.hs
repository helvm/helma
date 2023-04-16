module HelVM.HelMA.Automaton.Instruction.Extras.Constructors where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Instruction

-- | Constructors

-- | ISM

immediateBinaryI :: Integer -> BinaryOperation -> Instruction
immediateBinaryI i = ISM . SPure . Unary . UImmediate i

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
copyTI   = sal $ Indexed ITop Copy
discardI = sal Discard

copyII :: Index -> Instruction
copyII = manipulationII Copy

moveII :: Index -> Instruction
moveII = manipulationII Move

slideII :: Index -> Instruction
slideII = manipulationII Slide

manipulationII :: IndexedOperation -> Index -> Instruction
manipulationII op i = sal $ Indexed (IImmediate i) op

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
sal = ISM . SPure

sio :: IOInstruction -> Instruction
sio = ISM . SIO

-- | ICF

markNI :: Natural -> Instruction
markNI = ICF . Mark . MNatural

markSI :: Label -> Instruction
markSI = ICF . Mark . MArtificial

jumpTI :: Instruction
jumpTI = labeledT Jump

jumpII :: Natural -> Instruction
jumpII = labeledI Jump

callSI , jumpSI :: Label -> Instruction
callSI = labeledA Call
jumpSI = labeledA Jump

branchSwapI :: BranchTest -> Instruction
branchSwapI = ICF . Branch BSwapped

bNeTI :: Instruction
bNeTI = branchT NE

bNeII :: Natural -> Instruction
bNeII = branchI NE

bEzSI , bLtzSI :: Label -> Instruction
bEzSI  = branchA EZ
bLtzSI = branchA LTZ

branchT :: BranchTest -> Instruction
branchT = ICF . Branch BTop

branchI :: BranchTest -> Natural -> Instruction
branchI op n = ICF $ Branch (BImmediate n) op

branchA :: BranchTest -> Label -> Instruction
branchA op l = ICF $ Branch (BArtificial l) op

labeledT :: LabelOperation -> Instruction
labeledT = ICF . Labeled LTop

labeledI :: LabelOperation -> Natural -> Instruction
labeledI op n = ICF $ Labeled (LImmediate n) op

labeledA :: LabelOperation -> Label -> Instruction
labeledA op l = ICF $ Labeled (LArtificial l) op

returnI :: Instruction
returnI = ICF Return

-- | ILS

storeI , loadI :: Instruction
storeI = ILS Store
loadI  = ILS Load

mInputI , mInputDecI :: Instruction
mInputI    = mio InputChar
mInputDecI = mio InputDec

mio :: IOInstruction -> Instruction
mio = ILS . MIO

storeIDI :: Integer -> Index -> Instruction
storeIDI v = ILS . StoreID v

loadDI :: Index -> Instruction
loadDI = ILS . LoadD

moveDI :: Index -> Index -> Instruction
moveDI a = ILS . MoveD a
