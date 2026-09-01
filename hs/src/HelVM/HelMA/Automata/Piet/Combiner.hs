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
stepMemory targetColor oldMem = evalTransitionBlockMemory (currentColour oldMem) targetColor oldMem

-- TRANSITION EVALUATION

evalTransitionBlockMemory ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Memory → Memory → m Memory
evalTransitionBlockMemory (Just (Chromatic sourceColor)) targetColor oldMem = evalChromaticCommand sourceColor targetColor oldMem
evalTransitionBlockMemory _ _ _                                             = pure

evalChromaticCommand ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Memory → Memory → m Memory
evalChromaticCommand sourceColor targetColor = colors2Command sourceColor targetColor . blockCodelCount

-- COLOR DIFFERENCE & COMMAND EXECUTION

colors2Command ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Int → Memory → m Memory
colors2Command sourceColor targetColor = executeColorDiff $ chromaticDiff sourceColor targetColor

executeColorDiff ∷ AppSafeEff m ⇒ ChromaticColor → Int → Memory → m Memory
executeColorDiff (ChromaticColor Red    Light)   _ s = pure s
executeColorDiff (ChromaticColor Red    Normal)  n s = pietPush n s
executeColorDiff (ChromaticColor Red    Dark)    _ s = pietPop s
executeColorDiff (ChromaticColor Yellow Light)   _ s = pietAdd s
executeColorDiff (ChromaticColor Yellow Normal)  _ s = pietSubtract s
executeColorDiff (ChromaticColor Yellow Dark)    _ s = pietMultiply s
executeColorDiff (ChromaticColor Green  Light)   _ s = pietDivide s
executeColorDiff (ChromaticColor Green  Normal)  _ s = pietMod s
executeColorDiff (ChromaticColor Green  Dark)    _ s = pietNot s
executeColorDiff (ChromaticColor Cyan   Light)   _ s = pietGreater s
executeColorDiff (ChromaticColor Cyan   Normal)  _ s = pietPointer s
executeColorDiff (ChromaticColor Cyan   Dark)    _ s = pietSwitch s
executeColorDiff (ChromaticColor Blue   Light)   _ s = pietDuplicate s
executeColorDiff (ChromaticColor Blue   Normal)  _ s = pietRoll s
executeColorDiff (ChromaticColor Blue   Dark)    _ s = pietInNumber s
executeColorDiff (ChromaticColor Magenta Light)  _ s = pietInChar s
executeColorDiff (ChromaticColor Magenta Normal) _ s = pietOutNumber s
executeColorDiff (ChromaticColor Magenta Dark)   _ s = pietOutChar s
