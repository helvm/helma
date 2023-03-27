module HelVM.HelMA.Automaton.Combiner.ALU (
  runALI,
  runSAL,

  doOutputChar2,
  doInputChar2,
  doInputDec2,
  divMod,
  sub,
  binaryInstruction,
  binaryInstructions,
  halibut,
  move,
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
  Stack,
) where

import           HelVM.HelMA.Automaton.Instruction.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.SInstruction

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Containers.LLIndexSafe

import           HelVM.HelIO.ListLikeExtra

import           Control.Applicative.Tools
import           Data.ListLike                                   hiding (show)
import           Prelude                                         hiding (divMod, drop, fromList, length, splitAt, swap)


runALI :: ALU m ll element => SInstruction -> ll -> m ll
runALI (SPure ali) = runSAL ali
runALI (SIO ioi)   = runSIO ioi

runSIO :: ALU m ll element => IOInstruction -> ll -> m ll
runSIO OutputChar = doOutputChar2
runSIO OutputDec  = doOutputDec2
runSIO InputChar  = doInputChar2
runSIO InputDec   = doInputDec2

runSAL :: SafeStack m ll element => SPureInstruction -> ll -> m ll
runSAL (Cons      i   ) = push  i
runSAL (Unary     op  ) = unaryInstruction op
runSAL (Binary    op  ) = binaryInstruction op
runSAL (Binaries  ops ) = binaryInstructions ops
runSAL (Indexed t op)   = indexedInstruction op t
runSAL  Halibut         = halibut
runSAL  Pick            = pick
runSAL  Discard         = discard

-- | Arithmetic instructions
unaryInstruction :: SafeStack m ll element => UnaryOperation -> ll -> m ll
unaryInstruction (UImmediate i op) = build <.> pop1 where
  build (e , l) = push1 (calculateOp (fromInteger i) e op) l
unaryInstruction               op  = error $ show op

divMod :: SafeStack m ll element => ll -> m ll
divMod = binaryInstructions [Mod , Div]

sub :: SafeStack m ll element => ll -> m ll
sub = binaryInstruction Sub

binaryInstruction :: SafeStack m ll element => BinaryOperation -> ll -> m ll
binaryInstruction i = binaryInstructions [i]

binaryInstructions :: SafeStack m ll element => [BinaryOperation] -> ll -> m ll
binaryInstructions il = build <.> pop2 where
  build (e , e', l) = pushList (calculateOps e e' il) l

-- | IO instructions
doOutputChar2 :: ALU m ll element => ll -> m ll
doOutputChar2 = appendError "ALU.doOutputChar2" . build <=< pop1 where
  build (e , l) = wPutAsChar e $> l

doOutputDec2 :: ALU m ll element => ll -> m ll
doOutputDec2 = appendError "ALU.doOutputDec2" . build <=< pop1 where
  build (e , l) = wPutAsDec e $> l

doInputChar2 :: ALU m ll element => ll -> m ll
doInputChar2 l = appendError "ALU.doOutputDec2" $ build <$> wGetCharAs where
  build e = push1 e l

doInputDec2 :: ALU m ll element => ll -> m ll
doInputDec2 l = build <$> wGetCharAs where
  build e = push1 e l

indexedInstruction :: SafeStack m ll element => IndexedOperation -> IndexOperand -> ll -> m ll
indexedInstruction i ITop           = indexedInstructionTop i
indexedInstruction i (IImmediate n) = indexedInstructionImmediate i n

-- | Indexed instructions
indexedInstructionTop :: SafeStack m ll element => IndexedOperation -> ll -> m ll
indexedInstructionTop op = appendError "ALU.indexedInstructionTop" . build <=< unconsSafe where
  build (e , l) = indexedInstructionImmediate op (fromIntegral e) l

indexedInstructionImmediate :: SafeStack m ll element => IndexedOperation -> Index -> ll -> m ll
indexedInstructionImmediate Copy  = copy
indexedInstructionImmediate Move  = move
indexedInstructionImmediate Slide = slide

-- | Halibut and Pick instructions
halibut :: SafeStack m ll element => ll -> m ll
halibut = appendError "ALU.halibut" . build <=< pop1 where
  build (e , l)
    | 0 < i     = move i l
    | otherwise = copy (negate i) l
      where i = fromIntegral e

pick :: SafeStack m ll element => ll -> m ll
pick = appendError "ALU.pick" . build <=< pop1 where
  build (e , l)
    | 0 <= i    = copy i l
    | otherwise = move (negate i) l
      where i = fromIntegral e

-- | Slide instructions
slide :: SafeStack m ll element => Index -> ll -> m ll
slide i = appendError "ALU.pop2" . build <.> pop1 where
  build (e , l) = push1 e $ drop i l

-- | Move instructions
move :: SafeStack m ll element => Index -> ll -> m ll
move i l = build $ length l where
  build ll
    | ll <= i = liftErrorWithTupleList "ALU.move index must be less then lenght" [("i" , show i) , ("ll" , show ll)]
    | otherwise = pure $ l1 <> l2 <> l3 where
      (l1 , l3) = splitAt 1 l'
      (l2 , l') = splitAt i l

-- | Copy instructions
copy :: SafeStack m ll element => Index -> ll -> m ll
copy i = teeMap flipPush1 (findSafe i)

-- | Pop instructions
pop1 :: SafeStack m ll element => ll ->  m (element , ll)
pop1 = appendError "ALU.pop1" . unconsSafe

pop2 :: SafeStack m ll element => ll -> m (element , element , ll)
pop2 = appendError "ALU.pop2" . uncons2Safe

-- | Push instructions
push :: SafeStack m ll element => Integer -> ll -> m ll
push i = pure . genericPush1 i

flipPush1 :: Stack ll element => ll -> element -> ll
flipPush1 = flip push1

charPush1 :: (Num element , Stack ll element) => Char -> ll -> ll
charPush1 = genericPush1 . ord

genericPush1 :: (Integral v , Num element , Stack ll element) => v -> ll -> ll
genericPush1 = push1 . fromIntegral

push1 :: Stack ll element => element -> ll -> ll
push1 e = pushList [e]

push2 :: Stack ll element => element -> element -> ll -> ll
push2 e e' = pushList [e , e']

pushList :: Stack ll element => [element] -> ll -> ll
pushList es l = fromList es <> l

teeMap :: Functor f => (t -> a -> b) -> (t -> f a) -> t -> f b
teeMap f2 f1 x = f2 x <$> f1 x

-- | Types
type ALU m ll element = (BIO m , SafeStack m ll element)

type SafeStack m ll element  = (MonadSafe m , IntegralStack ll element)

type IntegralStack ll element = (Stack ll element , Integral element)

type Stack ll element = (Show ll , ListLike ll element , IndexSafe ll element)
