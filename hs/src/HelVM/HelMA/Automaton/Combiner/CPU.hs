module HelVM.HelMA.Automaton.Combiner.CPU where

import           HelVM.HelMA.Automaton.Combiner.ALU

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.Extras.Patterns
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction

import           HelVM.HelIO.Containers.MTIndexSafe
import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import qualified Data.Map.Strict                                        as Map
import qualified Data.Vector                                            as Vector

runCFI ∷ (ALU m ll element , Show element) ⇒ CFInstruction → CentralProcessingStep ll m
runCFI (Mark      _) = pure
runCFI (Branch  o t) = branchInstruction t o
runCFI (Labeled o i) = labeledInstruction i o
runCFI (Switch   ls) = switchInstruction ls
runCFI  Return       = popAddress

popAddress ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m $ CentralProcessingMemory ll
popAddress (CPM (CM il _ (IS (a : is)) nm am) s) = pure $ CPM (CM il a (IS is) nm am) s
popAddress (CPM (CM il _ (IS      [] ) _  _ ) _) = liftErrorWithTupleList "Empty Return Stack" [("il" , show il)]

cpmPop1 ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m (element , CentralProcessingMemory ll)
cpmPop1 (CPM cm s) = build <$> pop1 s where
   build (l , s') = (l , CPM cm s')

cpmPop2 ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m (element , element , CentralProcessingMemory ll)
cpmPop2 (CPM cm s) = build <$> pop2 s where
   build (l1 , l2 , s') = (l1 , l2 , CPM cm s')

--

switchInstruction ∷ ALU m ll element ⇒ NonEmpty Label → CentralProcessingStep ll m
switchInstruction ls = appendError "CPM.switchInstruction" . (buildSwitch ls <=< cpmPop1)

buildSwitch ∷ ALU m ll element ⇒ NonEmpty Label → (element , CentralProcessingMemory ll) → m (CentralProcessingMemory ll)
buildSwitch ls (e , cpm) = flip jump cpm <$> (flip findAddressForArtificialLabel cpm =<< indexSafe (toList ls) (mod (fromIntegral e) $ length ls))

--

branchInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → BranchOperand → CentralProcessingStep ll m
branchInstruction t  BSwapped       = branchSwappedInstruction    t
branchInstruction t  BTop           = branchTopInstruction        t
branchInstruction t (BImmediate  l) = branchImmediateInstruction  t l
branchInstruction t (BArtificial l) = branchArtificialInstruction t l

branchSwappedInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → CentralProcessingStep ll m
branchSwappedInstruction t cpm = appendError "CPM.branchSwappedInstruction" $ build =<< cpmPop2 cpm where
  build (e , l , cpm') = branch t e (findAddressForNaturalLabel l cpm') cpm'

branchTopInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → CentralProcessingStep ll m
branchTopInstruction t cpm = appendError "CPM.branchTopInstruction" $ build =<< cpmPop2 cpm where
  build (l , e , cpm') = branch t e (findAddressForNaturalLabel l cpm') cpm'

branchImmediateInstruction ∷ (ALU m ll element, DynamicLabel l) ⇒ BranchTest → l → CentralProcessingStep ll m
branchImmediateInstruction t l cpm = appendError "CPM.branchImmediateInstruction" $ build =<< cpmPop1 cpm where
  build (e , cpm') = branch t e (findAddressForNaturalLabel l cpm') cpm'

branchArtificialInstruction ∷ (ALU m ll element) ⇒ BranchTest → Label → CentralProcessingStep ll m
branchArtificialInstruction t l cpm = appendError "CPM.branchArtificialInstruction" $ build =<< cpmPop1 cpm where
  build (e , cpm') = branch t e (findAddressForArtificialLabel l cpm') cpm'

branch ∷ (ALU m ll element) ⇒ BranchTest → element → m InstructionCounter → CentralProcessingStep ll m
branch t e icM cpm
  | isJump t e = flip jump cpm <$> icM
  | otherwise  = pure cpm

--

labeledInstruction ∷ (ALU m ll element , Show element) ⇒ LabelOperation → LabelOperand → CentralProcessingStep ll m
labeledInstruction  i LTop            = labeledTopInstruction        i
labeledInstruction  i (LImmediate  l) = labeledImmediateInstruction  i l
labeledInstruction  i (LArtificial l) = labeledArtificialInstruction i l

labeledTopInstruction ∷ (ALU m ll element , Show element) ⇒ LabelOperation → CentralProcessingStep ll m
labeledTopInstruction i cpm = appendError "CPM.labeledTopInstruction" $ uncurry (labeledImmediateInstruction i) =<< cpmPop1 cpm

labeledImmediateInstruction ∷ (ALU m ll element, DynamicLabel l) ⇒ LabelOperation → l → CentralProcessingStep ll m
labeledImmediateInstruction i l cpm = appendError "CPM.labeledImmediateInstruction" $ flip (labeled i) cpm <$> findAddressForNaturalLabel l cpm

labeledArtificialInstruction ∷ ALU m ll element ⇒ LabelOperation → Label → CentralProcessingStep ll m
labeledArtificialInstruction i l cpm = appendError "CPM.labeledArtificialInstruction" $ flip (labeled i) cpm <$> findAddressForArtificialLabel l cpm

findAddressForNaturalLabel ∷ (MonadSafe m , DynamicLabel n) ⇒ n → CentralProcessingMemory ll → m InstructionAddress
findAddressForNaturalLabel n cpm
  | n < 0     = liftError $ show n
  | otherwise = liftMaybeOrErrorTuple ("Undefined label", show n) $ Map.lookup (fromIntegral n) (naturalLabelMap $ controlMemory cpm)

findAddressForArtificialLabel ∷ MonadSafe m ⇒ Label → CentralProcessingMemory ll → m InstructionAddress
findAddressForArtificialLabel l cpm = liftMaybeOrErrorTuple ("Undefined label", show l) $ Map.lookup l (artificialLabelMap $ controlMemory cpm)

--

labeled ∷ LabelOperation → InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
labeled Jump = jump
labeled Call = call

jump ∷ InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
jump a (CPM (CM il _ is nm am) s) = CPM (CM il a is nm am) s

call ∷ InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
call a (CPM (CM il ic (IS is) nm am) s) = CPM (CM il a (IS (ic : is)) nm am) s

-- | ControlMemory methods

newCM ∷ InstructionList → ControlMemory
newCM il = CM vec 0 (IS []) natMap artMap where
  vec = Vector.fromList il
  indexed = zip [0..] il
  natMap = Map.fromList [ (n, idx) | (idx, instr) <- indexed, Just n <- [extractNat instr] ]
  artMap = Map.fromList [ (l, idx) | (idx, instr) <- indexed, Just l <- [extractArt instr] ]

currentInstruction ∷ MonadSafe m ⇒ ControlMemory → m Instruction
currentInstruction (CM il ic _ _ _) = indexSafe il ic

incrementPC ∷ ControlMemory → ControlMemory
incrementPC cu = cu { programCounter = 1 + programCounter cu }

cpmProgram ∷ CentralProcessingMemory al → InstructionVector
cpmProgram = program . controlMemory

-- | Types
type DynamicLabel l = (Integral l , Show l)

type CentralProcessingStep ll m = CentralProcessingMemory ll → m $ CentralProcessingMemory ll

data CentralProcessingMemory ll
  = CPM
      { controlMemory :: ControlMemory
      , alm           :: ll
      }
  deriving stock (Show)

data ControlMemory
  = CM
      { program            :: InstructionVector
      , programCounter     :: InstructionCounter
      , returnStack        :: InstructionStack
      , naturalLabelMap    :: Map.Map Natural InstructionAddress
      , artificialLabelMap :: Map.Map Label InstructionAddress
      }
  deriving stock (Show)

newtype InstructionStack
  = IS [InstructionAddress]
  deriving stock (Show)

type InstructionCounter = InstructionAddress

type InstructionAddress = Int
