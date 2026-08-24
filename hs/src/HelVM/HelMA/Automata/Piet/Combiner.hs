module HelVM.HelMA.Automata.Piet.Combiner
  ( PreviousColor
  , applyPreviousColor
  , stepMemory
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automata.Piet.Combiner.ALU
import           HelVM.HelMA.Automata.Piet.Combiner.CPU

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           HelVM.HelMA.Automaton.Eff.MonadEff

type PreviousColor = (ChromaticColor, Int)

-- PUBLIC API

applyPreviousColor ∷ AppSafeEff m ⇒ Maybe PreviousColor → ChromaticColor → Memory → m Memory
applyPreviousColor (Just (c, s)) c' = colors2Command c c' s
applyPreviousColor Nothing    _     = pure

stepMemory ∷ AppSafeEff m ⇒ ChromaticColor → Memory → Memory → m Memory
stepMemory targetColor oldMem = evalTransitionBlock (currentColour oldMem) targetColor oldMem

-- CORE TRANSITION EVALUATION

evalTransitionBlock ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Memory → Memory → m Memory
evalTransitionBlock (Just (Chromatic sourceColor)) targetColor oldMem newMem = 
  evalChromaticCommand sourceColor targetColor oldMem newMem
evalTransitionBlock _ _ _ newMem = 
  pure newMem

evalChromaticCommand ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Memory → Memory → m Memory
evalChromaticCommand sourceColor targetColor oldMem = 
  colors2Command sourceColor targetColor (blockCodelCount (currentBlock oldMem) oldMem)

-- COLOR DIFFERENCE TO COMMAND MAPPING

colors2Command ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Int → Memory → m Memory
colors2Command sourceColor targetColor = 
  executeColorDiff $ diffColor sourceColor targetColor

executeColorDiff ∷ AppSafeEff m ⇒ ChromaticColor → Int → Memory → m Memory
executeColorDiff (ChromaticColor Light  Red)     _ s = pure s
executeColorDiff (ChromaticColor Normal Red)     n s = pietPush n s
executeColorDiff (ChromaticColor Dark   Red)     _ s = pietPop s
executeColorDiff (ChromaticColor Light  Yellow)  _ s = pietAdd s
executeColorDiff (ChromaticColor Normal Yellow)  _ s = pietSubtract s
executeColorDiff (ChromaticColor Dark   Yellow)  _ s = pietMultiply s
executeColorDiff (ChromaticColor Light  Green)   _ s = pietDivide s
executeColorDiff (ChromaticColor Normal Green)   _ s = pietMod s
executeColorDiff (ChromaticColor Dark   Green)   _ s = pietNot s
executeColorDiff (ChromaticColor Light  Cyan)    _ s = pietGreater s
executeColorDiff (ChromaticColor Normal Cyan)    _ s = pietPointer s
executeColorDiff (ChromaticColor Dark   Cyan)    _ s = pietSwitch s
executeColorDiff (ChromaticColor Light  Blue)    _ s = pietDuplicate s
executeColorDiff (ChromaticColor Normal Blue)    _ s = pietRoll s
executeColorDiff (ChromaticColor Dark   Blue)    _ s = pietInNumber s
executeColorDiff (ChromaticColor Light  Magenta) _ s = pietInChar s
executeColorDiff (ChromaticColor Normal Magenta) _ s = pietOutNumber s
executeColorDiff (ChromaticColor Dark   Magenta) _ s = pietOutChar s