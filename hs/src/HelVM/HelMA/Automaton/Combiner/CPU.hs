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
{-# INLINE runCFI #-}

popAddress ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m $ CentralProcessingMemory ll
popAddress (CPM (CM il _ (IS (a : is)) nm am) s) = pure $ CPM (CM il a (IS is) nm am) s
popAddress (CPM (CM il _ (IS      [] ) _  _ ) _) = liftErrorWithTupleList "Empty Return Stack" [("il" , show il)]
{-# INLINE popAddress #-}

cpmPop1 ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m (element , CentralProcessingMemory ll)
cpmPop1 (CPM cm s) = build <$> pop1 s where
   build (l , s') = (l , CPM cm s')
{-# INLINE cpmPop1 #-}

cpmPop2 ∷ ALU m ll element ⇒ CentralProcessingMemory ll → m (element , element , CentralProcessingMemory ll)
cpmPop2 (CPM cm s) = build <$> pop2 s where
   build (l1 , l2 , s') = (l1 , l2 , CPM cm s')
{-# INLINE cpmPop2 #-}

--

switchInstruction ∷ ALU m ll element ⇒ NonEmpty Label → CentralProcessingStep ll m
switchInstruction ls = appendError "CPM.switchInstruction" . (buildSwitch ls <=< cpmPop1)

buildSwitch ∷ ALU m ll element ⇒ NonEmpty Label → (element , CentralProcessingMemory ll) → m (CentralProcessingMemory ll)
buildSwitch ls (e , CPM cm s) = flip jump (CPM cm s) <$> (flip findAddressForArtificialLabel cm =<< indexSafe (toList ls) (mod (fromIntegral e) $ length ls))

--

branchInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → BranchOperand → CentralProcessingStep ll m
branchInstruction t  BSwapped       = branchSwappedInstruction    t
branchInstruction t  BTop           = branchTopInstruction        t
branchInstruction t (BImmediate  l) = branchImmediateInstruction  t l
branchInstruction t (BArtificial l) = branchArtificialInstruction t l
{-# INLINE branchInstruction #-}

branchSwappedInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → CentralProcessingStep ll m
branchSwappedInstruction t (CPM cm s) = build =<< pop2 s where
  build (e , l , s')
    | isJump t e = flip jump (CPM cm s') <$> findAddressForNaturalLabel l cm
    | otherwise  = pure $ CPM cm s'
{-# INLINE branchSwappedInstruction #-}

branchTopInstruction ∷ (ALU m ll element , Show element) ⇒ BranchTest → CentralProcessingStep ll m
branchTopInstruction t (CPM cm s) = build =<< pop2 s where
  build (l , e , s')
    | isJump t e = flip jump (CPM cm s') <$> findAddressForNaturalLabel l cm
    | otherwise  = pure $ CPM cm s'
{-# INLINE branchTopInstruction #-}

branchImmediateInstruction ∷ (ALU m ll element, DynamicLabel l) ⇒ BranchTest → l → CentralProcessingStep ll m
branchImmediateInstruction t l (CPM cm s) = build =<< pop1 s where
  build (e , s')
    | isJump t e = flip jump (CPM cm s') <$> findAddressForNaturalLabel l cm
    | otherwise  = pure $ CPM cm s'
{-# INLINE branchImmediateInstruction #-}

branchArtificialInstruction ∷ ALU m ll element ⇒ BranchTest → Label → CentralProcessingStep ll m
branchArtificialInstruction t l (CPM cm s) = build =<< pop1 s where
  build (e , s')
    | isJump t e = flip jump (CPM cm s') <$> findAddressForArtificialLabel l cm
    | otherwise  = pure $ CPM cm s'
{-# INLINE branchArtificialInstruction #-}

--

labeledInstruction ∷ (ALU m ll element , Show element) ⇒ LabelOperation → LabelOperand → CentralProcessingStep ll m
labeledInstruction  i LTop            = labeledTopInstruction        i
labeledInstruction  i (LImmediate  l) = labeledImmediateInstruction  i l
labeledInstruction  i (LArtificial l) = labeledArtificialInstruction i l
{-# INLINE labeledInstruction #-}

labeledTopInstruction ∷ (ALU m ll element , Show element) ⇒ LabelOperation → CentralProcessingStep ll m
labeledTopInstruction i (CPM cm s) = build =<< pop1 s where
  build (l , s') = flip (labeled i) (CPM cm s') <$> findAddressForNaturalLabel l cm
{-# INLINE labeledTopInstruction #-}

labeledImmediateInstruction ∷ (ALU m ll element, DynamicLabel l) ⇒ LabelOperation → l → CentralProcessingStep ll m
labeledImmediateInstruction i l (CPM cm s) = flip (labeled i) (CPM cm s) <$> findAddressForNaturalLabel l cm
{-# INLINE labeledImmediateInstruction #-}

labeledArtificialInstruction ∷ ALU m ll element ⇒ LabelOperation → Label → CentralProcessingStep ll m
labeledArtificialInstruction i l (CPM cm s) = flip (labeled i) (CPM cm s) <$> findAddressForArtificialLabel l cm
{-# INLINE labeledArtificialInstruction #-}

findAddressForNaturalLabel ∷ (MonadSafe m , DynamicLabel n) ⇒ n → ControlMemory → m InstructionAddress
findAddressForNaturalLabel n cm
  | n < 0     = liftError $ show n
  | otherwise = maybe (liftErrorTuple ("Undefined label", show n)) pure $ Map.lookup (fromIntegral n) cm.naturalLabelMap
{-# INLINE findAddressForNaturalLabel #-}

findAddressForArtificialLabel ∷ MonadSafe m ⇒ Label → ControlMemory → m InstructionAddress
findAddressForArtificialLabel l cm = maybe (liftErrorTuple ("Undefined label", show l)) pure $ Map.lookup l cm.artificialLabelMap
{-# INLINE findAddressForArtificialLabel #-}

--

labeled ∷ LabelOperation → InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
labeled Jump = jump
labeled Call = call
{-# INLINE labeled #-}

jump ∷ InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
jump a (CPM (CM il _ is nm am) s) = CPM (CM il a is nm am) s
{-# INLINE jump #-}

call ∷ InstructionCounter → CentralProcessingMemory ll → CentralProcessingMemory ll
call a (CPM (CM il ic (IS is) nm am) s) = CPM (CM il a (IS (ic : is)) nm am) s
{-# INLINE call #-}

-- | ControlMemory methods

newCM ∷ InstructionList → ControlMemory
newCM il = CM vec 0 (IS []) natMap artMap where
  vec = Vector.fromList il
  indexed = zip [0..] il
  natMap = Map.fromList [ (n, idx) | (idx, instr) <- indexed, Just n <- [extractNat instr] ]
  artMap = Map.fromList [ (l, idx) | (idx, instr) <- indexed, Just l <- [extractArt instr] ]

currentInstruction ∷ MonadSafe m ⇒ ControlMemory → m Instruction
currentInstruction (CM il ic _ _ _) = maybe err pure $ il Vector.!? ic where
  err = liftError "Instruction counter out of bounds"
{-# INLINE currentInstruction #-}

incrementPC ∷ ControlMemory → ControlMemory
incrementPC (CM il ic is nm am) = CM il (ic + 1) is nm am
{-# INLINE incrementPC #-}

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
      { program            :: !InstructionVector
      , programCounter     :: {-# UNPACK #-} !InstructionCounter
      , returnStack        :: !InstructionStack
      , naturalLabelMap    :: !(Map.Map Natural InstructionAddress)
      , artificialLabelMap :: !(Map.Map Label InstructionAddress)
      }
  deriving stock (Show)

newtype InstructionStack
  = IS [InstructionAddress]
  deriving stock (Show)

type InstructionCounter = InstructionAddress

type InstructionAddress = Int
