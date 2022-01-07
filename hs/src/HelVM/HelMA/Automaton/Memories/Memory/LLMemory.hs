module HelVM.HelMA.Automaton.Memories.Memory.LLMemory where

import           HelVM.HelMA.Automaton.Memories.RAM.LLRAM
import           HelVM.HelMA.Automaton.Memories.Stack.LLStack

import           HelVM.HelMA.Automaton.BIntegral

import           HelVM.Common.Control.Safe

import           Control.Type.Operator

loadMemory :: (MonadSafe m , Stack s element , RAM r element, BIntegral element) => Memory s r -> m $ Memory s r
loadMemory (Memory s r) = load' <$> pop1 s where
  load' (e , _) = Memory (push1 (load r $ fromIntegral e) s) r

storeMemory :: (MonadSafe m , Stack s element , RAM r element, BIntegral element) => Memory s r -> m $ Memory s r
storeMemory (Memory s r) = store' <$> pop2 s where
  store' (e , e' , s') = Memory s' $ store e  e' r

data Memory s r = Memory s r
