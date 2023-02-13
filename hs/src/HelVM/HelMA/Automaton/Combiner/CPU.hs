module HelVM.HelMA.Automaton.Combiner.CPU where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelMA.Automaton.Combiner.ALU

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.ListLike                                   hiding (show)
import qualified Data.Vector                                     as Vector

runCFI :: (ALU m ll element , Show element) => CFInstruction -> CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
runCFI (DMark    _          ) = pure
runCFI (SMark    _          ) = pure
runCFI  Return                = popAddress
runCFI (CDynamic   Call     ) = dynamicCall
runCFI (CDynamic   Jump     ) = dynamicJump
runCFI (CDynamic  (Branch t)) = dynamicBranch  t
runCFI (CStatic l  Call     ) = staticCall   l
runCFI (CStatic l  Jump     ) = staticJump   l
runCFI (CStatic l (Branch t)) = staticBranch l t

popAddress :: ALU m ll element  => CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
popAddress (CPM (CM il _ (IS (a : is))) s) = pure $ CPM (CM il a $ IS is) s
popAddress (CPM (CM il _ (IS      [] )) _) = liftErrorWithTupleList "Empty Return Stack" [("il" , show il)]

dynamicCall :: (ALU m ll element , Show element) => CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
dynamicCall (CPM (CM il ic (IS is)) s) = appendError "CPM.dynamicCall" $ call1 =<< pop1 s where
  call1 (n , s') = call2 <$> findAddressForDynamicLabel n il where
    call2 a = CPM (CM il a (IS (ic : is))) s'

dynamicJump :: (ALU m ll element , Show element) => CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
dynamicJump (CPM (CM il _ is) s) = appendError "CPM.dynamicJump" $ jump1 =<< pop1 s where
  jump1 (n, s') = jump2 <$> findAddressForDynamicLabel n il where
    jump2 a = CPM (CM il a is) s'

dynamicBranch :: (ALU m ll element , Show element) => BranchTest -> CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
dynamicBranch t (CPM (CM il ic is) s) = appendError "CPM.dynamicBranch" $ branch =<< pop2 s where
  branch (n , e , s')
    | isNotJump t e = pure $ CPM (CM il ic is) s'
    | otherwise     = jump <$> findAddressForDynamicLabel n il where
      jump ic' = CPM (CM il ic' is) s'

findAddressForDynamicLabel :: (MonadSafe m , Integral n , Show n) => n -> InstructionVector -> m InstructionAddress
findAddressForDynamicLabel n il
  | n < 0     = liftError $ show n
  | otherwise = liftMaybeOrErrorTuple ("Undefined label", show n) $ findIndex (isDMark $ fromIntegral n) il

--

staticCall :: ALU m ll element => Label -> CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
staticCall l (CPM (CM il ic (IS is)) s) = appendError "CPM.staticCall" $ call <$> findAddressForStaticLabel l il where
  call a = CPM (CM il a (IS (ic : is))) s

staticJump :: ALU m ll element => Label -> CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
staticJump l (CPM (CM il _  is) s) = appendError "CPM.staticJump" $ jump <$> findAddressForStaticLabel l il where
  jump a = CPM (CM il a is) s

staticBranch :: ALU m ll element => Label -> BranchTest -> CentralProcessingMemory ll -> m $ CentralProcessingMemory ll
staticBranch l t (CPM (CM il ic is) s) = appendError "CPM.staticBranch" $ branch =<< pop1 s where
  branch (e , s')
    | isNotJump t e = pure $ CPM (CM il ic is) s'
    | otherwise     = jump <$> findAddressForStaticLabel l il where
      jump ic' = CPM (CM il ic' is) s'

findAddressForStaticLabel :: MonadSafe m => Label -> InstructionVector -> m InstructionAddress
findAddressForStaticLabel l = liftMaybeOrErrorTuple ("Undefined label", show l) . findIndex (isSMark l)

-- | ControlMemory methods

newCM :: InstructionList -> ControlMemory
newCM il = CM (Vector.fromList il) 0 (IS [])

currentInstruction :: MonadSafe m => ControlMemory -> m Instruction
currentInstruction (CM il ic _) = indexSafe il ic

incrementPC :: ControlMemory -> ControlMemory
incrementPC cu = cu { programCounter = 1 + programCounter cu }

-- | Types
data CentralProcessingMemory al = CPM
  { controlMemory :: ControlMemory
  , alm           :: al
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
