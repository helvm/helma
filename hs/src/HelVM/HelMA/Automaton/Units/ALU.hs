module HelVM.HelMA.Automaton.Units.ALU (
  doOutputChar2,
  doInputChar2,
  doInputDec2,
  divMod,
  sub,
  binaryInstruction,
  binaryInstructions,
  halibut,
  alInstruction,
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
  Stack,
) where

import           HelVM.HelMA.Automaton.Instruction.ALInstruction
import           HelVM.HelMA.Automaton.Instruction.IOInstruction

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Containers.LLIndexSafe

import           HelVM.HelIO.ListLikeExtra

import           Control.Applicative.Tools
import           Data.ListLike                                   hiding (show)
import           Prelude                                         hiding (divMod, drop, fromList, splitAt, swap)


alInstruction :: ALU m ll element => ALInstruction -> ll -> m ll
alInstruction (Cons    i   )   = push  i
alInstruction (Unary     op)   = error $ show op
alInstruction (Binary    op)   = binaryInstruction op
alInstruction (Binaries  ops)  = binaryInstructions ops
alInstruction (SDynamic  op)   = dynamicManipulation op
alInstruction (SStatic i op)   = staticManipulation op i
alInstruction (SIO OutputChar) = doOutputChar2
alInstruction (SIO OutputDec)  = doOutputDec2
alInstruction (SIO InputChar)  = doInputChar2
alInstruction (SIO InputDec)   = doInputDec2
alInstruction  Halibut         = halibut
alInstruction  Pick            = pick
alInstruction  Discard         = discard


-- | Arithmetic instructions
divMod :: ALU m ll element => ll -> m ll
divMod = binaryInstructions [Mod , Div]

sub :: ALU m ll element => ll -> m ll
sub = binaryInstruction Sub

binaryInstruction :: ALU m ll element => BinaryInstruction -> ll -> m ll
binaryInstruction i = binaryInstructions [i]

binaryInstructions :: ALU m ll element => [BinaryInstruction] -> ll -> m ll
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

-- | Manipulation instructions
dynamicManipulation :: ALU m ll element => ManipulationInstruction -> ll -> m ll
dynamicManipulation op = appendError "ALU.dynamicManipulation" . build <=< unconsSafe where
  build (e , l) = staticManipulation op (fromIntegral e) l

staticManipulation :: ALU m ll element => ManipulationInstruction -> Index -> ll -> m ll
staticManipulation Copy  = copy
staticManipulation Move  = move
staticManipulation Slide = slide

-- | Halibut and Pick instructions
halibut :: ALU m ll element => ll -> m ll
halibut = appendError "ALU.halibut" . build <=< pop1 where
  build (e , l)
    | 0 < i     = move i l
    | otherwise = copy (negate i) l
      where i = fromIntegral e

pick :: ALU m ll element => ll -> m ll
pick = appendError "ALU.pick" . build <=< pop1 where
  build (e , l)
    | 0 <= i    = copy i l
    | otherwise = move (negate i) l
      where i = fromIntegral e

-- | Slide instructions
slide :: ALU m ll element => Index -> ll -> m ll
slide i = appendError "ALU.pop2" . build <.> pop1 where
  build (e , l) = push1 e $ drop i l

-- | Move instructions
move :: ALU m ll element => Index -> ll -> m ll
move i l = pure $ l1 <> l2 <> l3 where
  (l1 , l3) = splitAt 1 l'
  (l2 , l') = splitAt i l

-- | Copy instructions
copy :: ALU m ll element => Index -> ll -> m ll
copy i = teeMap flipPush1 (findSafe i)

-- | Pop instructions
pop1 :: ALU m ll element => ll ->  m (element , ll)
pop1 = appendError "ALU.pop1" . unconsSafe

pop2 :: ALU m ll element => ll -> m (element , element , ll)
pop2 = appendError "ALU.pop2" . uncons2Safe

-- | Push instructions
push :: ALU m ll element => Integer -> ll -> m ll
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
type ALU m ll element = (BIO m , Stack ll element , Integral element)

type Stack ll element = (Show ll , ListLike ll element , IndexSafe ll element)

type Index = Int
