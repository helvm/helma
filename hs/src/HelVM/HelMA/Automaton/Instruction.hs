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

dupI , swapI , rotI , dCopy , discardI :: Instruction
dupI     = sCopyI 0
swapI    = sMoveI 1
rotI     = sMoveI 2
dCopy    = sal $ SDynamic Copy
discardI = sal Discard

sCopyI :: StackIndex -> Instruction
sCopyI = sStatic Copy

sMoveI :: StackIndex -> Instruction
sMoveI = sStatic Move

sSlideI :: StackIndex -> Instruction
sSlideI = sStatic Slide

sStatic :: ManipulationInstruction -> StackIndex -> Instruction
sStatic i = sal . flip SStatic i

sInputI , sOutputI , sOutputDecI :: Instruction
sInputI     = sio InputChar
sOutputI    = sio OutputChar
sOutputDecI = sio OutputDec

binaries :: [BinaryInstruction] -> Instruction
binaries = sal . Binaries

binary :: BinaryInstruction -> Instruction
binary = sal . Binary

unary :: UnaryInstruction -> Instruction
unary = sal . Unary

sal :: ALInstruction -> Instruction
sal = IAL . SAL

sio :: IOInstruction -> Instruction
sio = IAL . SIO

dMarkI :: Natural -> Instruction
dMarkI = ICF . DMark

sMarkIN :: Natural -> Instruction
sMarkIN = sMarkI . show

sMarkI :: Label -> Instruction
sMarkI = ICF . SMark

sJumpIN :: Natural -> Instruction
sJumpIN = cStaticI Jump . show

sCallI , sJumpI :: Label -> Instruction
sCallI = cStaticI Call
sJumpI = cStaticI Jump

sEZI , sLTZI :: Label -> Instruction
sEZI  = cStaticI (Branch EZ )
sLTZI = cStaticI (Branch LTZ)

cStaticI :: LabelInstruction -> Label -> Instruction
cStaticI i label = ICF $ CStatic label i

returnI :: Instruction
returnI = ICF Return

storeI , loadI :: Instruction
storeI = ILS Store
loadI = ILS Load

mInputI , mInputDecI :: Instruction
mInputI    = ILS (MIO InputChar)
mInputDecI = ILS (MIO InputDec )

-- | Others

extractPureIAL :: Instruction -> Maybe ALInstruction
extractPureIAL (IAL (SAL i)) = Just i
extractPureIAL           _   = Nothing

isPureIAL :: Instruction -> Bool
isPureIAL (IAL (SIO _)) = False
isPureIAL (IAL      _ ) = True
isPureIAL           _   = False

isICF :: Instruction -> Bool
isICF (ICF _) = True
isICF      _  = False

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
    IAL !SInstruction
  | ILS !LSInstruction
  | ICF !CFInstruction
  | End
  | Transfer
  deriving stock (Eq , Read , Show)

type InstructionList   = [Instruction]
type InstructionVector = Vector Instruction
