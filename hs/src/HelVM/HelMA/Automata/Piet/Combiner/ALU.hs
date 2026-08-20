module HelVM.HelMA.Automata.Piet.Combiner.ALU
  ( -- | I/O Instructions
    pietInChar
  , pietInNumber
  , pietOutChar
  , pietOutNumber
    -- | Stack & Arithmetic Instructions
  , pietAdd
  , pietDivide
  , pietDuplicate
  , pietGreater
  , pietMod
  , pietMultiply
  , pietNot
  , pietPop
  , pietPush
  , pietRoll
  , pietSubtract
  ) where

import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automaton.Combiner.ALU                     hiding ( Stack )
import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Prelude                                                hiding ( getLine )

-- | I/O Instructions
pietInNumber ∷ AppSafeEff m ⇒ Memory → m Memory
pietInNumber = modifyStack "in_number" inputDec

pietInChar ∷ AppSafeEff m ⇒ Memory → m Memory
pietInChar = modifyStack "in_char" inputChar

pietOutNumber ∷ AppSafeEff m ⇒ Memory → m Memory
pietOutNumber = modifyStack "out_number" outputDecMaybe

pietOutChar ∷ AppSafeEff m ⇒ Memory → m Memory
pietOutChar = modifyStack "out_char" outputCharMaybe

-- | Push / Pop
pietPush ∷ AppSafeEff m ⇒ Int → Memory → m Memory
pietPush n = modifyStack ("push " <> show n) (pure . push1 n)

pietPop ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietPop = modifyStack "pop" discard

-- | Binary & Unary Arithmetic Instructions
pietAdd ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietAdd = modifyStack "add" (binaryInstruction Add)

pietSubtract ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietSubtract = modifyStack "subtract" (binaryInstruction Sub)

pietMultiply ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietMultiply = modifyStack "multiply" (binaryInstruction Mul)

pietDivide ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietDivide = modifyStack "divide" (binaryInstruction Div)

pietMod ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietMod = modifyStack "mod" (binaryInstruction Mod)

pietNot ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietNot = modifyStack "not" lNot

pietGreater ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietGreater = modifyStack "greater" (binaryInstruction LGT)

-- | Stack Manipulation Instructions
pietDuplicate ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietDuplicate = modifyStack "duplicate" (copy 0)

pietRoll ∷ (ALU m Stack Int) ⇒ Memory → m Memory
pietRoll = modifyStack "roll" roll

-- | Utils
modifyStack ∷ AppSafeEff m ⇒ Text → (Stack → m Stack) → Memory → m Memory
modifyStack name f (Memory im s) = logWithPosition name im *> (Memory im <$> f s)
