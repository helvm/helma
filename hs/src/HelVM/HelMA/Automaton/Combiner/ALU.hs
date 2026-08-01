module HelVM.HelMA.Automaton.Combiner.ALU (
  runALI,
  runSAL,

  outputCharMaybe,
  outputDecMaybe,

  outputChar,
  outputDec,
  inputChar,
  inputDec,
  lNot,
  divMod,
  sub,
  binaryInstruction,
  binaryInstructions,
  roll,
  halibut,
  move,
  rollStatic,
  discard,
  slide,
  copy,
  flipPush1,
  charPush1,
  genericPush1,
  pop1,
  pop2,
  push1,
  push2,
  splitAt,
  drop,
  ALU,
  SafeStack,
  IntegralStack,
  Stack,
) where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common

import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Containers.MTIndexSafe

import           HelVM.HelIO.SequencesExtra

import           Control.Applicative.Tools

import           Data.Char                                              (ord)
import           Data.MonoTraversable                                   (Element, olength)
import           Data.Sequences                                         hiding (Vector)
import           Prelude                                                hiding (divMod, drop, splitAt, uncons, fromList)


runALI :: ALU m ll => SMInstruction -> ll -> m ll
runALI (SPure ali) = runSAL ali
runALI (SIO   ioi) = runSIO ioi

runSIO :: ALU m ll => IOInstruction -> ll -> m ll
runSIO OutputChar = outputChar
runSIO OutputDec  = outputDec
runSIO InputChar  = inputChar
runSIO InputDec   = inputDec

runSAL :: SafeStack m ll => SPureInstruction -> ll -> m ll
runSAL (Cons      i   ) = push  i
runSAL (Unary     op  ) = unaryInstruction op
runSAL (Binary    op  ) = binaryInstruction op
runSAL (Binaries  ops ) = binaryInstructions ops
runSAL (Indexed t op)   = indexedInstruction op t
runSAL  Halibut         = halibut
runSAL  Pick            = pick
runSAL  Discard         = discard

-- | Arithmetic instructions
unaryInstruction :: SafeStack m ll => UnaryOperation -> ll -> m ll
unaryInstruction (UImmediate i op) = build <.> pop1 where
  build (e , l) = push1 (calculateOp (fromInteger i) e op) l
unaryInstruction  LNot             = lNot
unaryInstruction               op  = error $ show op

lNot :: SafeStack m ll => ll -> m ll
lNot = build <.> pop1 where
  build (e , l) = push1 (go e) l
  go 0 = 1
  go _ = 0

divMod :: SafeStack m ll => ll -> m ll
divMod = binaryInstructions [Mod , Div]

sub :: SafeStack m ll => ll -> m ll
sub = binaryInstruction Sub

binaryInstruction :: SafeStack m ll => BinaryOperation -> ll -> m ll
binaryInstruction i = binaryInstructions [i]

binaryInstructions :: SafeStack m ll => [BinaryOperation] -> ll -> m ll
binaryInstructions il = build <.> pop2 where
  build (e , e', l) = pushList (calculateOps e e' il) l

-- | IO instructions
outputCharMaybe :: ALU m ll => ll -> m ll
outputCharMaybe = appendError "ALU.outputCharMaybe" . outputMaybe putAsChar

outputDecMaybe :: ALU m ll => ll -> m ll
outputDecMaybe = appendError "ALU.outputDecMaybe" . outputMaybe putAsDec

outputMaybe :: ALU m ll => (Element ll -> m ()) -> ll -> m ll
outputMaybe putAs l = maybe (pure l) f (uncons l) where f = uncurry $ flip (<$) . putAs

outputChar :: ALU m ll => ll -> m ll
outputChar = appendError "ALU.outputChar" . build <=< pop1 where
  build (e , l) = putAsChar e $> l

outputDec :: ALU m ll => ll -> m ll
outputDec = appendError "ALU.outputDec" . build <=< pop1 where
  build (e , l) = putAsDec e $> l

inputChar :: ALU m ll => ll -> m ll
inputChar l = appendError "ALU.inputChar" $ build <$> getCharAs where
  build e = push1 e l

inputDec :: ALU m ll => ll -> m ll
inputDec l = appendError "ALU.inputDec" $ build <$> getDecAs where
  build e = push1 e l

indexedInstruction :: SafeStack m ll => IndexedOperation -> IndexOperand -> ll -> m ll
indexedInstruction i ITop           = indexedInstructionTop i
indexedInstruction i (IImmediate n) = indexedInstructionImmediate i n

-- | Indexed instructions
indexedInstructionTop :: SafeStack m ll => IndexedOperation -> ll -> m ll
indexedInstructionTop op = appendError "ALU.indexedInstructionTop" . build <=< unconsSafe where
  build (e , l) = indexedInstructionImmediate op (fromIntegral e) l

indexedInstructionImmediate :: SafeStack m ll => IndexedOperation -> Int -> ll -> m ll
indexedInstructionImmediate Copy  = copy
indexedInstructionImmediate Move  = move
indexedInstructionImmediate Slide = slide

-- | Halibut and Pick instructions

roll :: SafeStack m ll => ll -> m ll
roll = build <=< pop2 where
  build (rolls, depth, l) = rollStatic (fromIntegral rolls) (fromIntegral depth) l

halibut :: SafeStack m ll => ll -> m ll
halibut = appendError "ALU.halibut" . build <=< pop1 where
  build (e , l)
    | 0 < i     = move i l
    | otherwise = copy (negate i) l
      where i = fromIntegral e

pick :: SafeStack m ll => ll -> m ll
pick = appendError "ALU.pick" . build <=< pop1 where
  build (e , l)
    | 0 <= i    = copy i l
    | otherwise = move (negate i) l
      where i = fromIntegral e

-- | Slide instructions
slide :: SafeStack m ll => Int -> ll -> m ll
slide i = appendError "ALU.pop2" . build <.> pop1 where
  build (e , l) = push1 e $ drop (fromIntegral i) l

move :: SafeStack m ll => Int -> ll -> m ll
move i = rollStatic i (i + 1)

rollStatic :: SafeStack m ll => Int -> Int -> ll -> m ll
rollStatic rolls i l = build $ fromIntegral (olength l) where
  build ll
    | i < 0     = pure l
    | r == 0    = pure l
    | ll < i    = liftErrorWithTupleList "ALU.role index must be less then lenght" [("i" , show i) , ("ll" , show ll)]
    | otherwise = pure $ l1 <> l2 <> l3
    where
      r = rolls `mod` i
      (l2, l1) = splitAt (fromIntegral r) l'
      (l', l3) = splitAt (fromIntegral i) l

-- | Copy instructions
copy :: SafeStack m ll => Int -> ll -> m ll
copy i = teeMap flipPush1 (findSafe i)

-- | Pop instructions
pop1 :: SafeStack m ll => ll -> m (Element ll , ll)
pop1 = appendError "ALU.pop1" . unconsSafe

pop2 :: SafeStack m ll => ll -> m (Element ll , Element ll , ll)
pop2 = appendError "ALU.pop2" . uncons2Safe

-- | Push instructions
push :: SafeStack m ll => Integer -> ll -> m ll
push i = pure . genericPush1 i

flipPush1 :: Stack ll => ll -> Element ll -> ll
flipPush1 = flip push1

charPush1 :: Stack ll => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (Integral v , Stack ll) => v -> ll -> ll
genericPush1 = push1 . fromIntegral

push1 :: Stack ll => Element ll -> ll -> ll
push1 e = pushList [e]

push2 :: Stack ll => Element ll -> Element ll -> ll -> ll
push2 e e' = pushList [e , e']

pushList :: Stack ll => [Element ll] -> ll -> ll
pushList es l = fromList es <> l

teeMap :: Functor f => (t -> a -> b) -> (t -> f a) -> t -> f b
teeMap f2 f1 x = f2 x <$> f1 x

-- | Types
type ALU m ll = (AppEff m, SafeStack m ll)

type SafeStack m ll = (MonadSafe m, IntegralStack ll)

type IntegralStack ll = (Stack ll, Integral (Element ll))

type Stack ll =
  ( Show ll
  , IsSequence ll
  , IndexSafe ll
  )
