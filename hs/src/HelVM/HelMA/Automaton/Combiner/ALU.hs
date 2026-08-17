module HelVM.HelMA.Automaton.Combiner.ALU
  ( ALU
  , SafeStack
  , Stack
  , binaryInstruction
  , binaryInstructions
  , charPush1
  , copy
  , discard
  , divMod
  , drop
  , flipPush1
  , genericPush1
  , halibut
  , inputChar
  , inputDec
  , lNot
  , move
  , outputChar
  , outputCharMaybe
  , outputDec
  , outputDecMaybe
  , pop1
  , pop2
  , push1
  , push2
  , roll
  , rollImediate
  , runALI
  , runSAL
  , slide
  , splitAt
  , sub
  ) where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common

import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Containers.MTIndexSafe
import           HelVM.HelIO.SequencesExtra

import           Control.Applicative.Tools
import           Data.MonoTraversable
import           Data.Sequences
import           Prelude                                                hiding ( divMod, drop, fromList, length, splitAt, swap, uncons )


runALI ∷ ALU m ll element ⇒ SMInstruction → ll → m ll
runALI (SPure ali) = runSAL ali
runALI (SIO   ioi) = runSIO ioi

runSIO ∷ ALU m ll element ⇒ IOInstruction → ll → m ll
runSIO OutputChar = outputChar
runSIO OutputDec  = outputDec
runSIO InputChar  = inputChar
runSIO InputDec   = inputDec

runSAL ∷ SafeStack m ll element ⇒ SPureInstruction → ll → m ll
runSAL (Cons      i   ) = push  i
runSAL (Unary     op  ) = unaryInstruction op
runSAL (Binary    op  ) = binaryInstruction op
runSAL (Binaries  ops ) = binaryInstructions ops
runSAL (Indexed t op)   = indexedInstruction op t
runSAL  Halibut         = halibut
runSAL  Pick            = pick
runSAL  Discard         = discard

-- | Arithmetic instructions
unaryInstruction ∷ SafeStack m ll element ⇒ UnaryOperation → ll → m ll
unaryInstruction (UImmediate i op) = build <.> pop1 where
  build (e , l) = push1 (calculateOp (fromInteger i) e op) l
unaryInstruction  LNot             = lNot
unaryInstruction               op  = error $ show op

lNot ∷ SafeStack m ll element ⇒ ll → m ll
lNot = build <.> pop1 where
  build (e , l) = push1 (go e) l
  go 0 = 1
  go _ = 0

divMod ∷ SafeStack m ll element ⇒ ll → m ll
divMod = binaryInstructions [Mod , Div]

sub ∷ SafeStack m ll element ⇒ ll → m ll
sub = binaryInstruction Sub

binaryInstruction ∷ SafeStack m ll element ⇒ BinaryOperation → ll → m ll
binaryInstruction i = binaryInstructions [i]

binaryInstructions ∷ SafeStack m ll element ⇒ [BinaryOperation] → ll → m ll
binaryInstructions il = build <.> pop2 where
  build (e , e', l) = pushList (calculateOps e e' il) l

-- | IO instructions
outputCharMaybe ∷ ALU m ll element ⇒ ll → m ll
outputCharMaybe = appendError "ALU.outputCharMaybe" . outputMaybe putAsChar

outputDecMaybe ∷ ALU m ll element ⇒ ll → m ll
outputDecMaybe = appendError "ALU.outputDecMaybe" .outputMaybe putAsDec

outputMaybe ∷ ALU m ll element ⇒ (element → m ()) → ll → m ll
outputMaybe putAs l = maybe (pure l) f (uncons l) where f = uncurry $ flip (<$) . putAs

outputChar ∷ ALU m ll element ⇒ ll → m ll
outputChar = appendError "ALU.outputChar" . build <=< pop1 where
  build (e , l) = putAsChar e $> l

outputDec ∷ ALU m ll element ⇒ ll → m ll
outputDec = appendError "ALU.outputDec" . build <=< pop1 where
  build (e , l) = putAsDec e $> l

inputChar ∷ ALU m ll element ⇒ ll → m ll
inputChar l = appendError "ALU.inputChar" $ build <$> getCharAs where
  build e = push1 e l

inputDec ∷ ALU m ll element ⇒ ll → m ll
inputDec l = appendError "ALU.inputDec" $ build <$> getDecAs where
  build e = push1 e l

indexedInstruction ∷ SafeStack m ll element ⇒ IndexedOperation → IndexOperand → ll → m ll
indexedInstruction i ITop           = indexedInstructionTop i
indexedInstruction i (IImmediate n) = indexedInstructionImmediate i n

-- | Indexed instructions
indexedInstructionTop ∷ SafeStack m ll element ⇒ IndexedOperation → ll → m ll
indexedInstructionTop op = appendError "ALU.indexedInstructionTop" . build <=< unconsSafe where
  build (e , l) = indexedInstructionImmediate op (fromIntegral e) l

indexedInstructionImmediate ∷ SafeStack m ll element ⇒ IndexedOperation → ImmediateIndex → ll → m ll
indexedInstructionImmediate Copy  = copy
indexedInstructionImmediate Move  = move
indexedInstructionImmediate Slide = slide

-- | Halibut and Pick instructions

roll ∷ SafeStack m ll element ⇒ ll → m ll
roll = build <=< pop2 where
  build (rolls, depth, l) = rollImediate (fromIntegral rolls) (fromIntegral depth) l

halibut ∷ SafeStack m ll element ⇒ ll → m ll
halibut = appendError "ALU.halibut" . build <=< pop1 where
  build (e , l)
    | 0 < i     = move i l
    | otherwise = copy (negate i) l
      where i = fromIntegral e

pick ∷ SafeStack m ll element ⇒ ll → m ll
pick = appendError "ALU.pick" . build <=< pop1 where
  build (e , l)
    | 0 <= i    = copy i l
    | otherwise = move (negate i) l
      where i = fromIntegral e

-- | Slide instructions
slide ∷ SafeStack m ll element ⇒ ImmediateIndex → ll → m ll
slide i = appendError "ALU.pop2" . build <.> pop1 where
  build (e , l) = push1 e $ drop i l

move ∷ SafeStack m ll element ⇒ ImmediateIndex → ll → m ll
move i = rollImediate i (i + 1)

rollImediate ∷ SafeStack m ll element ⇒ ImmediateIndex → ImmediateIndex → ll → m ll
rollImediate rolls i l = build $ olength l where
  build ll
    | i < 0     = pure l
    | r == 0    = pure l
    | ll < i    = liftErrorWithTupleList "ALU.role index must be less then lenght" [("i" , show i) , ("ll" , show ll)]
    | otherwise = pure $ l1 <> l2 <> l3
    where
      r = rolls `mod` i
      (l2, l1) = splitAt r l'
      (l', l3) = splitAt i l

-- | Copy instructions
copy ∷ SafeStack m ll element ⇒ ImmediateIndex → ll → m ll
copy i = teeMap flipPush1 (findSafe i)

-- | Pop instructions
pop1 ∷ SafeStack m ll element ⇒ ll →  m (element , ll)
pop1 = appendError "ALU.pop1" . unconsSafe

pop2 ∷ SafeStack m ll element ⇒ ll → m (element , element , ll)
pop2 = appendError "ALU.pop2" . uncons2Safe

-- | Push instructions
push ∷ SafeStack m ll element ⇒ Integer → ll → m ll
push i = pure . genericPush1 i

flipPush1 ∷ Stack ll element ⇒ ll → element → ll
flipPush1 = flip push1

charPush1 ∷ (Num element , Stack ll element) ⇒ Char → ll → ll
charPush1 = genericPush1 . ord

genericPush1 ∷ (Integral v , Num element , Stack ll element) ⇒ v → ll → ll
genericPush1 = push1 . fromIntegral

push1 ∷ Stack ll element ⇒ element → ll → ll
push1 e = pushList [e]

push2 ∷ Stack ll element ⇒ element → element → ll → ll
push2 e e' = pushList [e , e']

pushList ∷ Stack ll element ⇒ [element] → ll → ll
pushList es l = fromList es <> l

teeMap ∷ Functor f ⇒ (t → a → b) → (t → f a) → t → f b
teeMap f2 f1 x = f2 x <$> f1 x

-- | Types
type ALU m ll element = (AppEff m , SafeStack m ll element)

type SafeStack m ll element  = (MonadSafe m , IntegralStack ll element)

type IntegralStack ll element = (Stack ll element , Integral element)

type Stack ll element = (Show ll , IsSequence ll , Element ll ~ element , Index ll ~ Int , IndexSafe ll)
