module HelVM.HelMA.Automaton.Combiner.CPU where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelMA.Automaton.Combiner.ALU

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.ListLike                                   hiding (show)
import qualified Data.Vector                                     as Vector

runCFI :: (ALU m ll element , Show element) => CFInstruction -> CentralProcessingStep ll m
runCFI (Mark    _                ) = pure
runCFI (Labeled LTop            i) = topInstruction        i
runCFI (Labeled (LImmediate  l) i) = immediateInstruction  i l
runCFI (Labeled (LArtificial l) i) = artificialInstruction i l
runCFI  Return                     = popAddress

popAddress :: ALU m ll element  => CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
popAddress (CPM (CM il _ (IS (a : is))) s) = pure $ CPM (CM il a $ IS is) s
popAddress (CPM (CM il _ (IS      [] )) _) = liftErrorWithTupleList "Empty Return Stack" [("il" , show il)]

--

topInstruction :: (ALU m ll element , Show element) => LabeledOperation -> CentralProcessingStep ll m
topInstruction i cpm = appendError "CPM.topInstruction" $ uncurry (immediateInstruction i) =<< cpmPop1 cpm

immediateInstruction :: (ALU m ll element, DynamicLabel l) => LabeledOperation -> l -> CentralProcessingStep ll m
immediateInstruction i l cpm = appendError "CPM.immediateInstruction" $ flip (labeled i) cpm =<< findAddressForNaturalLabel l (cpmProgram cpm)

artificialInstruction :: ALU m ll element => LabeledOperation -> Label -> CentralProcessingStep ll m
artificialInstruction i l cpm = appendError "CPM.artificialInstruction" $ flip (labeled i) cpm =<< findAddressForArtificialLabel l (cpmProgram cpm)

--

findAddressForNaturalLabel :: (MonadSafe m , DynamicLabel n) => n -> InstructionVector -> m InstructionAddress --FIXME
findAddressForNaturalLabel n il
  | n < 0     = liftError $ show n
  | otherwise = liftMaybeOrErrorTuple ("Undefined label", show n) $ findIndex (checkNaturalMark $ fromIntegral n) il

findAddressForArtificialLabel :: MonadSafe m => Label -> InstructionVector -> m InstructionAddress
findAddressForArtificialLabel l = liftMaybeOrErrorTuple ("Undefined label", show l) . findIndex (checkArtificialMark l)

--

labeled :: ALU m ll element => LabeledOperation -> InstructionCounter -> CentralProcessingStep ll m
labeled (Branch t) = branch t
labeled Jump       = jump
labeled Call       = call

branch :: ALU m ll element => BranchTest -> InstructionCounter -> CentralProcessingStep ll m
branch t ic cpm = build <$> cpmPop1 cpm where
  build (e , cpm')
    | isNotJump t e = cpm'
    | otherwise     = doJump ic cpm'

doJump :: InstructionCounter -> CentralProcessingMemory ll -> CentralProcessingMemory ll
doJump ic (CPM (CM il _ is) s) = CPM (CM il ic is) s

jump :: (Applicative m) => InstructionCounter -> CentralProcessingStep ll m
jump a (CPM (CM il _  is) s) = pure $ CPM (CM il a is) s

call :: (Applicative m) => InstructionCounter -> CentralProcessingStep ll m
call a (CPM (CM il ic (IS is)) s) = pure $ CPM (CM il a (IS (ic : is))) s

-- | ControlMemory methods

newCM :: InstructionList -> ControlMemory
newCM il = CM (Vector.fromList il) 0 (IS [])

currentInstruction :: MonadSafe m => ControlMemory -> m Instruction
currentInstruction (CM il ic _) = indexSafe il ic

incrementPC :: ControlMemory -> ControlMemory
incrementPC cu = cu { programCounter = 1 + programCounter cu }

cpmProgram :: CentralProcessingMemory al -> InstructionVector
cpmProgram = program . controlMemory

cpmPop1 :: ALU m ll element => CentralProcessingMemory ll -> m (element , CentralProcessingMemory ll)
cpmPop1 (CPM cm s) = build <$> pop1 s where
   build (l , s') = (l , CPM cm s')

cpmPop2 :: ALU m ll element => CentralProcessingMemory ll -> m (element , element , CentralProcessingMemory ll)
cpmPop2 (CPM cm s) = build <$> pop2 s where
   build (l1 , l2 , s') = (l1 , l2 , CPM cm s')

-- | Types
type DynamicLabel l = (Integral l , Show l)

type CentralProcessingStep ll m = CentralProcessingMemory ll -> m $ CentralProcessingMemory ll

data CentralProcessingMemory ll = CPM
  { controlMemory :: ControlMemory
  , alm           :: ll
  }
  deriving stock (Show)

data ControlMemory = CM
  { program        :: InstructionVector
  , programCounter :: InstructionCounter
  , returnStack    :: InstructionStack
  }
  deriving stock (Show)

newtype InstructionStack = IS [InstructionAddress]
  deriving stock (Show)

type InstructionCounter = InstructionAddress

type InstructionAddress = Int
