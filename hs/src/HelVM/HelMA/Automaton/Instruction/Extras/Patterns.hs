{-# LANGUAGE PatternSynonyms #-}
module HelVM.HelMA.Automaton.Instruction.Extras.Patterns where

import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           HelVM.HelMA.Automaton.Instruction

-- | Getters

isICF ∷ Instruction → Bool
isICF (ICF _) = True
isICF      _  = False
{-# INLINE isICF #-}

isMark ∷ Instruction → Bool
isMark (MarkP _) = True
isMark        _  = False
{-# INLINE isMark #-}

checkNaturalMark ∷ Natural → Instruction → Bool
checkNaturalMark n (MNaturalP n') = n == n'
checkNaturalMark _            _   = False
{-# INLINE checkNaturalMark #-}

checkArtificialMark ∷ Label → Instruction → Bool
checkArtificialMark l (MArtificialP l') = l == l'
checkArtificialMark _               _   = False
{-# INLINE checkArtificialMark #-}

-- | Patterns

-- | ISM

pattern SubP :: Instruction
pattern SubP = ISM (SPure (Binary Sub))
{-# INLINE SubP #-}

pattern HalibutP :: Instruction
pattern HalibutP = ISM (SPure Halibut)
{-# INLINE HalibutP #-}

pattern PickP :: Instruction
pattern PickP = ISM (SPure Pick)
{-# INLINE PickP #-}

pattern ConsP :: Integer → Instruction
pattern ConsP c = ISM (SPure (Cons c))
{-# INLINE ConsP #-}

pattern CopyIP :: ImmediateIndex → Instruction
pattern CopyIP i = ISM (SPure (Indexed (IImmediate i) Copy))
{-# INLINE CopyIP #-}

pattern MoveIP :: ImmediateIndex → Instruction
pattern MoveIP i = ISM (SPure (Indexed (IImmediate i) Move))
{-# INLINE MoveIP #-}

pattern AddIP :: Integer → Instruction
pattern AddIP i = ISM (SPure (Unary (UImmediate i Add)))
{-# INLINE AddIP #-}

pattern SubIP :: Integer → Instruction
pattern SubIP i = ISM (SPure (Unary (UImmediate i Sub)))
{-# INLINE SubIP #-}

pattern BinaryP :: BinaryOperation → Instruction
pattern BinaryP op = ISM (SPure (Binary op))
{-# INLINE BinaryP #-}

pattern SPureP :: SPureInstruction → Instruction
pattern SPureP i = ISM (SPure i)
{-# INLINE SPureP #-}

-- | ICF

pattern BNeIP :: Natural → Instruction
pattern BNeIP i = ICF (Branch (BImmediate i) NE)
{-# INLINE BNeIP #-}

pattern JumpP :: LabelOperand → Instruction
pattern JumpP o = ICF (Labeled o Jump)
{-# INLINE JumpP #-}

pattern MarkP :: Mark → Instruction
pattern MarkP m = ICF (Mark m)
{-# INLINE MarkP #-}

pattern MNaturalP :: Natural → Instruction
pattern MNaturalP n = ICF (Mark (MNatural n))
{-# INLINE MNaturalP #-}

pattern MArtificialP :: Label → Instruction
pattern MArtificialP l = ICF (Mark (MArtificial l))
{-# INLINE MArtificialP #-}

pattern BranchTP :: BranchTest → Instruction
pattern BranchTP t = ICF (Branch BTop t)
{-# INLINE BranchTP #-}

-- | ILS

pattern StoreP :: Instruction
pattern StoreP = ILS Store
{-# INLINE StoreP #-}

pattern RStoreP :: Instruction
pattern RStoreP = ILS RStore
{-# INLINE RStoreP #-}

pattern LoadP :: Instruction
pattern LoadP = ILS Load
{-# INLINE LoadP #-}

pattern LoadDP :: ImmediateIndex → Instruction
pattern LoadDP a  = ILS (LoadD a)
{-# INLINE LoadDP #-}
