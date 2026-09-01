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
colors2Command sourceColor targetColor = executeColorChange $ chromaticChange sourceColor targetColor

executeColorChange ∷ AppSafeEff m ⇒ ChromaticColor → Int → Memory → m Memory
executeColorChange (ChromaticColor Red    Light)   _ s = pure s
executeColorChange (ChromaticColor Red    Normal)  n s = pietPush n s
executeColorChange (ChromaticColor Red    Dark)    _ s = pietPop s
executeColorChange (ChromaticColor Yellow Light)   _ s = pietAdd s
executeColorChange (ChromaticColor Yellow Normal)  _ s = pietSubtract s
executeColorChange (ChromaticColor Yellow Dark)    _ s = pietMultiply s
executeColorChange (ChromaticColor Green  Light)   _ s = pietDivide s
executeColorChange (ChromaticColor Green  Normal)  _ s = pietMod s
executeColorChange (ChromaticColor Green  Dark)    _ s = pietNot s
executeColorChange (ChromaticColor Cyan   Light)   _ s = pietGreater s
executeColorChange (ChromaticColor Cyan   Normal)  _ s = pietPointer s
executeColorChange (ChromaticColor Cyan   Dark)    _ s = pietSwitch s
executeColorChange (ChromaticColor Blue   Light)   _ s = pietDuplicate s
executeColorChange (ChromaticColor Blue   Normal)  _ s = pietRoll s
executeColorChange (ChromaticColor Blue   Dark)    _ s = pietInNumber s
executeColorChange (ChromaticColor Magenta Light)  _ s = pietInChar s
executeColorChange (ChromaticColor Magenta Normal) _ s = pietOutNumber s
executeColorChange (ChromaticColor Magenta Dark)   _ s = pietOutChar s
