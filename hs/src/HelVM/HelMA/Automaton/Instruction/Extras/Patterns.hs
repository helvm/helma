{-# LANGUAGE PatternSynonyms #-}
module HelVM.HelMA.Automaton.Instruction.Extras.Patterns where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
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

-- | ISM

pattern SubP :: Instruction
pattern SubP = ISM (SPure (Binary Sub))

pattern HalibutP :: Instruction
pattern HalibutP = ISM (SPure Halibut)

pattern PickP :: Instruction
pattern PickP = ISM (SPure Pick)

pattern ConsP :: Integer -> Instruction
pattern ConsP c = ISM (SPure (Cons c))

pattern CopyIP :: Index -> Instruction
pattern CopyIP i = ISM (SPure (Indexed (IImmediate i) Copy))

pattern MoveIP :: Index -> Instruction
pattern MoveIP i = ISM (SPure (Indexed (IImmediate i) Move))

pattern BinaryP :: BinaryOperation -> Instruction
pattern BinaryP op = ISM (SPure (Binary op))

pattern SPureP :: SPureInstruction -> Instruction
pattern SPureP i = ISM (SPure i)

-- | ICF

pattern BNeIP :: Natural -> Instruction
pattern BNeIP i = ICF (Branch (BImmediate i) NE)

pattern JumpP :: LabelOperand -> Instruction
pattern JumpP o = ICF (Labeled o Jump)

pattern MarkP :: Mark -> Instruction
pattern MarkP m = ICF (Mark m)

pattern MNaturalP :: Natural -> Instruction
pattern MNaturalP n = ICF (Mark (MNatural n))

pattern MArtificialP :: Label -> Instruction
pattern MArtificialP l = ICF (Mark (MArtificial l))

pattern BranchTP :: BranchTest -> Instruction
pattern BranchTP t = ICF (Branch BTop t)

-- | ILS

pattern StoreP :: Instruction
pattern StoreP = ILS Store

pattern LoadP :: Instruction
pattern LoadP = ILS Load

pattern LoadDP :: Index -> Instruction
pattern LoadDP a  = ILS (LoadD a)
