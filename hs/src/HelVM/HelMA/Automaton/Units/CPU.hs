module HelVM.HelMA.Automaton.Units.CPU where

import           HelVM.HelMA.Automaton.Units.ALU

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelIO.Control.Safe

import           Control.Type.Operator

import           Data.ListLike                                   hiding (show)

controlInstruction :: (ALU m ll element , Show element) => CFInstruction -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
--controlInstruction :: ALU m ll element => CFInstruction -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
controlInstruction (Mark    _           ) = pure
controlInstruction  Return                = popAddress
controlInstruction (CDynamic   Call     ) = dynamicCall
controlInstruction (CDynamic   Jump     ) = dynamicJump
controlInstruction (CDynamic  (Branch t)) = dynamicBranch  t
controlInstruction (CStatic l  Call     ) = staticCall   l
controlInstruction (CStatic l  Jump     ) = staticJump   l
controlInstruction (CStatic l (Branch t)) = staticBranch l t

popAddress :: ALU m ll element  => CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
popAddress (CPU (CU il _ (IS (a : is))) s) = pure $ CPU (CU il a $ IS is) s
popAddress (CPU (CU il _ (IS      [] )) _) = liftErrorWithTupleList "Empty Return Stack" [("il" , show il)]

dynamicCall :: ALU m ll element => CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
dynamicCall (CPU (CU il ic (IS is)) s) = call <$> pop1 s where
  call (a , s') = CPU (CU il (fromIntegral a) (IS (ic : is))) s'

dynamicJump :: ALU m ll element => CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
dynamicJump (CPU (CU il _ is) s) = jump <$> pop1 s where
  jump (a , s') = CPU (CU il (fromIntegral a) is) s'

dynamicBranch :: (ALU m ll element , Show element) => BranchTest -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
--dynamicBranch :: ALU m ll element => BranchTest -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
dynamicBranch t (CPU (CU il ic is) s) = branch =<< pop2 s where
--  dynamicBranch' (l , e , s') = liftErrorTupleList [("t" , show t) , ("il" , show il) , ("ic" , show ic), ("is" , show is) , ("l" , show l) , ("e" , show e) , ("s'", show s')]
  branch (l , e , s')
    | isNotJump t e = liftErrorWithTupleList "isNotJump" [("t" , show t) , ("il" , show il) , ("ic" , show ic), ("is" , show is) , ("l" , show l) , ("e" , show e) , ("s'", show s')]
--    | isNotJump t e = pure $ CPU (CU il ic is) s'
    | otherwise     = liftErrorWithTupleList "otherwise" [("t" , show t) , ("il" , show il) , ("ic" , show ic), ("is" , show is) , ("l" , show l) , ("e" , show e) , ("s'", show s')]
--    | otherwise     = jump <$> findAddress il (show l) where jump ic' = CPU (CU il ic' is) s'

staticCall :: ALU m ll element => Label -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
staticCall l (CPU (CU il ic (IS is)) s) = call <$> findAddress il l where
  call a = CPU (CU il a (IS (ic : is))) s

staticJump :: ALU m ll element => Label -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
staticJump l (CPU (CU il _  is) s) = jump <$> findAddress il l where
  jump a = CPU (CU il a is) s

staticBranch :: ALU m ll element => Label -> BranchTest -> CentralProcessingUnit ll -> m $ CentralProcessingUnit ll
staticBranch l t (CPU (CU il ic is) s) = branch =<< pop1 s where
  branch (e , s')
    | isNotJump t e = pure $ CPU (CU il ic is) s'
    | otherwise     = jump <$> findAddress il l where
      jump ic' = CPU (CU il ic' is) s'

findAddress :: MonadSafe m => InstructionVector -> Label -> m InstructionAddress
findAddress il l = liftMaybeOrErrorTuple ("Undefined label", show l) $ findIndex (isMark l) il

cpuToTuple :: CentralProcessingUnit s -> (ControlUnit , s)
cpuToTuple (CPU cu s) = (cu , s)

-- | Types
data CentralProcessingUnit al = CPU
  { controlUnit :: ControlUnit
  , alu         :: al
  }
  deriving stock (Show)

data ControlUnit = CU
  { program        :: InstructionVector
  , programCounter :: InstructionCounter
  , returnStack    :: InstructionStack
  }
  deriving stock (Show)

newtype InstructionStack = IS [InstructionAddress]
  deriving stock (Show)

type InstructionCounter = InstructionAddress

type InstructionAddress = Int
