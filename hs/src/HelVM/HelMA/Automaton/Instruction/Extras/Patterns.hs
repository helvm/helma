{-# LANGUAGE PatternSynonyms #-}
module HelVM.HelMA.Automaton.Instruction.Extras.Patterns where

import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Instruction

-- | Getters

isICF :: Instruction -> Bool
isICF (ICF _) = True
isICF      _  = False

isMark :: Instruction -> Bool
isMark (MarkP _) = True
isMark        _  = False

checkNaturalMark :: Natural -> Instruction -> Bool
checkNaturalMark n (MNaturalP n') = n == n'
checkNaturalMark _            _   = False

checkArtificialMark :: Label -> Instruction -> Bool
checkArtificialMark l (MArtificialP l') = l == l'
checkArtificialMark _               _   = False

-- | Patterns
pattern JumpP :: LabelOperand -> Instruction
pattern JumpP o = ICF (Labeled o Jump)

pattern MarkP :: Mark -> Instruction
pattern MarkP m = (ICF (Mark m))

pattern HalibutP :: Instruction
pattern HalibutP = IAL (SPure Halibut)

pattern PickP :: Instruction
pattern PickP = IAL (SPure Pick)

pattern MNaturalP :: Natural -> Instruction
pattern MNaturalP n = (ICF (Mark (MNatural n)))

pattern MArtificialP :: Label -> Instruction
pattern MArtificialP l = (ICF (Mark (MArtificial l)))

pattern ConsP :: Integer -> Instruction
pattern ConsP c = IAL (SPure (Cons c))

pattern MoveIP :: Index -> Instruction
pattern MoveIP i = IAL (SPure (Indexed (IImmediate i) Move))

pattern BranchTP :: BranchTest -> Instruction
pattern BranchTP t = ICF (Branch BTop t)

pattern BinaryP :: BinaryOperation -> Instruction
pattern BinaryP op = IAL (SPure (Binary op))

pattern SPureP :: SPureInstruction -> Instruction
pattern SPureP i = IAL (SPure i)
