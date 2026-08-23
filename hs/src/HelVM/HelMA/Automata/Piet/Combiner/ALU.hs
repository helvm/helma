module HelVM.HelMA.Automata.Piet.Combiner.ALU
  ( pietAdd
  , pietDivide
  , pietDuplicate
  , pietGreater
  , pietInChar
  , pietInNumber
  , pietMod
  , pietMultiply
  , pietNot
  , pietOutChar
  , pietOutNumber
  , pietPop
  , pietPush
  , pietRoll
  , pietSubtract
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automaton.Combiner.ALU                     hiding ( Stack )
import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Prelude                                                hiding ( getLine )



-- | I/O Instructions
pietInNumber ∷ AppSafeEff m ⇒ Memory → m Memory
pietInNumber = modifyStackWithLog "in_number" inputDec

pietInChar ∷ AppSafeEff m ⇒ Memory → m Memory
pietInChar = modifyStackWithLog "in_char" inputChar

pietOutNumber ∷ AppSafeEff m ⇒ Memory → m Memory
pietOutNumber = modifyStackWithLog "out_number" outputDecMaybe

pietOutChar ∷ AppSafeEff m ⇒ Memory → m Memory
pietOutChar = modifyStackWithLog "out_char" outputCharMaybe

-- | Push / Pop
pietPush ∷ AppSafeEff m ⇒ Int → Memory → m Memory
pietPush n = modifyStackWithLog ("push " <> show n) (pure . push1 n)

pietPop ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietPop = modifyStackWithLog "pop" discard

-- | Binary & Unary Arithmetic Instructions
pietAdd ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietAdd = modifyStackWithLog "add" (binaryInstruction Add)

pietSubtract ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietSubtract = modifyStackWithLog "subtract" (binaryInstruction Sub)

pietMultiply ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietMultiply = modifyStackWithLog "multiply" (binaryInstruction Mul)

pietDivide ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietDivide = modifyStackWithLog "divide" (binaryInstruction Div)

pietMod ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietMod = modifyStackWithLog "mod" (binaryInstruction Mod)

pietNot ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietNot = modifyStackWithLog "not" lNot

pietGreater ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietGreater = modifyStackWithLog "greater" (binaryInstruction LGT)

-- | Stack Manipulation Instructions
pietDuplicate ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietDuplicate = modifyStackWithLog "duplicate" (copy 0)

pietRoll ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietRoll = modifyStackWithLog "roll" roll
