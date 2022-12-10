module HelVM.HelMA.Automaton.Evaluator (
  eval,
) where

import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.IO.EvaluatorIO

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.CFInstruction

import           HelVM.HelMA.Automaton.Units.ALU                 as Stack
import           HelVM.HelMA.Automaton.Units.CPU                 as CPU
import           HelVM.HelMA.Automaton.Units.LSU                 as LSU
import           HelVM.HelMA.Automaton.Units.Unit

import           Control.Monad.Extra
import           Control.Type.Operator

import           Data.Either.Extra

import           Prelude                                         hiding (swap)

eval :: (SREvaluator Symbol s r m) => Maybe Natural -> Unit s r -> m $ Unit s r
eval Nothing  = evalWithoutLimit
eval (Just n) = evalWithLimit n

evalWithLimit :: (SREvaluator Symbol s r m) => Natural -> Unit s r -> m $ Unit s r
evalWithLimit n u = loopM (actMWithLimit nextUnit) (n , u)

actMWithLimit :: Monad m => (a -> m $ Both a) -> WithLimit a -> m (Either (WithLimit a) a)
actMWithLimit f (n , x) = checkN n where
  checkN 0 = pure $ Right x
  checkN _ = mapLeft (n - 1 , ) <$> f x

evalWithoutLimit ::  (SREvaluator Symbol s r m) => Unit s r -> m $ Unit s r
evalWithoutLimit = loopM nextUnit

----

nextUnit :: (SREvaluator Symbol s r m) => Unit s r -> m $ UnitBoth s r
nextUnit u@(Unit cu _ _) = nextUnitForInstruction =<< currentInstruction cu where
  nextUnitForInstruction i = doInstruction i $ incrementIC u

----

doInstruction :: (SREvaluator Symbol s r m) => Instruction -> Unit s r -> m $ UnitBoth s r
doInstruction (IAL      i) u = Left . updateStack   u <$> alInstruction i (unitStack u)
doInstruction (ILS      i) u = Left . updateFromLSU u <$> slInstruction i (toLSU u)
doInstruction (ICF      i) u = Left . updateFromCPU u <$> controlInstruction i (toCPU u)
doInstruction  Transfer    u =  transfer u
doInstruction  End         u =  end u

transfer :: (SREvaluator Symbol s r m) => Unit s r -> m $ UnitBoth s r
transfer u = branch =<< pop2ForStack u where
  branch (_ , 0 , u') = pure $ Left u'
  branch (0 , _ , u') = end u'
  branch (_ , _ , u') = Left . updateFromCPU u' <$> controlInstruction dJumpI (toCPU u')

pop2ForStack :: (SREvaluator Symbol s r m) => Unit s r -> m (Symbol , Symbol , Unit s r)
pop2ForStack u = build <$> pop2 (unitStack u) where
  build (s1 , s2 , s') = (s1 , s2 , updateStack u s')

end :: (SREvaluator Symbol s r m) => Unit s r -> m $ UnitBoth s r
end = pure . Right

--teeMap :: Functor f => (t -> a -> b) -> (t -> f a) -> t -> f b
--teeMap f2 f1 x = f2 x <$> f1 x

type WithLimit a = (Natural , a)

type UnitBoth s r = Both (Unit s r)

type Both a = Either a a
