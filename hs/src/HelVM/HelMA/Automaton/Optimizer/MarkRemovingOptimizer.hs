module HelVM.HelMA.Automaton.Optimizer.MarkRemovingOptimizer (
  makrRemoving,
) where

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Instruction.Extras.Constructors
import           HelVM.HelMA.Automaton.Instruction.Extras.Patterns

import qualified Data.Set                                              as Set

makrRemoving :: InstructionList -> InstructionList
makrRemoving il = catMaybes $ makrRemovingWithSet set <$> il where set = consValueSet il

makrRemovingWithSet :: Set Integer -> Instruction -> Maybe Instruction
makrRemovingWithSet set (MNaturalP i) = mark set i
makrRemovingWithSet _              i  = Just i

mark :: Set.Set Integer -> Natural -> Maybe Instruction
mark set i = build $ Set.member (fromIntegral i) set where
  build True  = Just $ markNI i
  build False = Nothing

consValueSet :: InstructionList -> Set.Set Integer
consValueSet il = fromList $ catMaybes $ consValueOpt <$> il

consValueOpt :: Instruction -> Maybe Integer
consValueOpt (ConsP i) = Just i
consValueOpt        _  = Nothing
