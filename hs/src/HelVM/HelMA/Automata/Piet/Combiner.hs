module HelVM.HelMA.Automata.Piet.Combiner
  ( PreviousColor
  , applyPreviousColor
  , bbb
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automata.Piet.Combiner.ALU
import           HelVM.HelMA.Automata.Piet.Combiner.CPU

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           HelVM.HelMA.Automaton.Eff.MonadEff

applyPreviousColor ∷ AppSafeEff m ⇒ Maybe PreviousColor → ChromaticColor → Memory → m Memory
applyPreviousColor (Just (c, s)) c' = colors2Command c c' s
applyPreviousColor Nothing    _     = pure

bbb ∷ AppSafeEff m  ⇒ ChromaticColor → Memory → Memory → m Memory
bbb c' oldMem = evalTransitionBlockMemory (currentColour oldMem) c' oldMem

evalTransitionBlockMemory ∷ AppSafeEff m ⇒ Maybe Color → ChromaticColor → Memory → Memory → m Memory
evalTransitionBlockMemory (Just (Chromatic c)) c' mem mem' = aaa c c' mem mem'
evalTransitionBlockMemory _ _ _ mem'                       = pure mem'

aaa ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Memory → Memory → m Memory
aaa c c' mem = colors2Command c c' (blockCodelCount mem)

colors2Command ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Int → Memory → m Memory
colors2Command c c' = colorDiff2Command $ diffColor c c'

colorDiff2Command ∷ AppSafeEff m ⇒ ChromaticColor → Int → Memory → m Memory
colorDiff2Command (ChromaticColor Light  Red)     _ s = pure s
colorDiff2Command (ChromaticColor Normal Red)     n s = pietPush n s
colorDiff2Command (ChromaticColor Dark   Red)     _ s = pietPop s
colorDiff2Command (ChromaticColor Light  Yellow)  _ s = pietAdd s
colorDiff2Command (ChromaticColor Normal Yellow)  _ s = pietSubtract s
colorDiff2Command (ChromaticColor Dark   Yellow)  _ s = pietMultiply s
colorDiff2Command (ChromaticColor Light  Green)   _ s = pietDivide s
colorDiff2Command (ChromaticColor Normal Green)   _ s = pietMod s
colorDiff2Command (ChromaticColor Dark   Green)   _ s = pietNot s
colorDiff2Command (ChromaticColor Light  Cyan)    _ s = pietGreater s
colorDiff2Command (ChromaticColor Normal Cyan)    _ s = pietPointer s
colorDiff2Command (ChromaticColor Dark   Cyan)    _ s = pietSwitch s
colorDiff2Command (ChromaticColor Light  Blue)    _ s = pietDuplicate s
colorDiff2Command (ChromaticColor Normal Blue)    _ s = pietRoll s
colorDiff2Command (ChromaticColor Dark   Blue)    _ s = pietInNumber s
colorDiff2Command (ChromaticColor Light  Magenta) _ s = pietInChar s
colorDiff2Command (ChromaticColor Normal Magenta) _ s = pietOutNumber s
colorDiff2Command (ChromaticColor Dark   Magenta) _ s = pietOutChar s

type PreviousColor = (ChromaticColor, Int)

