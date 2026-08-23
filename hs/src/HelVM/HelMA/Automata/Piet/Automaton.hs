module HelVM.HelMA.Automata.Piet.Automaton
  ( interpret
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Memory

import           HelVM.HelMA.Automata.Piet.Combiner.ALU
import           HelVM.HelMA.Automata.Piet.Combiner.CPU

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Image
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.InstructionMemory
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Labelling          as Labelling
import           HelVM.HelMA.Automata.Piet.Types.Lightness
import           HelVM.HelMA.Automata.Piet.Types.Program            as Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline                   as Trampoline

import           HelVM.HelIO.Control.Safe

import           Data.IntMap                                        hiding ( filter )

import           Lens.Micro.Platform

-- Main interpreter entry point

interpret ∷ AppSafeEff m ⇒ Program → m ()
interpret prog = Trampoline.trampolineM interpretStep initialState where
  initialState = (NormalStep Nothing, initialMemory prog)

interpretStep ∷ AppSafeEff m ⇒ (StepState, Memory) → m (Either () InterpreterMemory)
interpretStep (NormalStep prev, mem) = stepNormal prev mem
interpretStep (WhiteStep limit, mem) = pure $ stepWhite limit mem

-- Step handlers

stepNormal ∷ AppSafeEff m ⇒ Maybe PreviousColor → Memory → m (Either () InterpreterMemory)
stepNormal previous memory = evalPixel (currentPixel memory) previous memory

stepWhite ∷ Int → Memory → Either () InterpreterMemory
stepWhite limit memory
  | limit <= 0 = Trampoline.break ()
  | otherwise  = Trampoline.continue $ checkWhitePixel (currentPixel memory) limit memory

-- Pixel handlers

evalPixel ∷ AppSafeEff m ⇒ Color → Maybe PreviousColor → Memory → m (Either () InterpreterMemory)
evalPixel (Chromatic color) previous mem = evalChromaticPixel previous color mem
evalPixel White             _        mem = pure $ Trampoline.continue $ evalWhitePixel mem
evalPixel Black             _        _   = liftError "Entered black block, terminate"

evalChromaticPixel ∷ AppSafeEff m ⇒ Maybe PreviousColor → ChromaticColor → Memory → m (Either () InterpreterMemory)
evalChromaticPixel previous color mem = makeNext <$> applyPreviousColor previous color mem where
  makeNext mem1 = handleNext (nonBlackSucc (programMemory mem1) mStats (orientationMemory mem1)) (color, getLabelSize mStats) mem1
  mStats   = getMaskInfo (programMemory mem) (positionMemory mem)

evalWhitePixel ∷ Memory → InterpreterMemory
evalWhitePixel mem = (WhiteStep whiteLimit, mem) where
  whiteLimit = 8 * getLabelSize (getMaskInfo (programMemory mem) (positionMemory mem))

checkWhitePixel ∷ Color → Int → Memory → InterpreterMemory
checkWhitePixel White limit = checkWhitePixelStep limit
checkWhitePixel _     _     = (NormalStep Nothing, )

checkWhitePixelStep ∷ Int → Memory → InterpreterMemory
checkWhitePixelStep limit mem = (WhiteStep (limit - 1), stepWhitePixel mem)

-- Helper functions

applyPreviousColor ∷ AppSafeEff m ⇒ Maybe PreviousColor → ChromaticColor → Memory → m Memory
applyPreviousColor (Just (oldColor, oldS)) color = colors2Command oldColor color oldS
applyPreviousColor Nothing                 _     = pure

getMaskInfo ∷ Program → Coordinates → Maybe LabelInfo
getMaskInfo prog pos = findWithDefault Nothing (pixelImage pos maskImg) infoMap
  where
    maskImg = prog ^. Program.labelling . Labelling.mask
    infoMap = prog ^. Program.labelling . Labelling.info

handleNext ∷ Maybe InstructionCounter → PreviousColor → Memory → Either () InterpreterMemory
handleNext (Just ic) prevColor mem = Trampoline.continue $ handleNextSuccess ic prevColor mem
handleNext Nothing           _   _ = Trampoline.break ()

handleNextSuccess ∷ InstructionCounter → PreviousColor → Memory → InterpreterMemory
handleNextSuccess ic prevColor mem =
  ( NormalStep (Just prevColor)
  , setInstructionCounter ic mem
  )

colors2Command ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticColor → Int → Memory → m Memory
colors2Command fromColor toColor = colorDiff2Command $ diffColor fromColor toColor

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

data StepState
  = NormalStep (Maybe PreviousColor)
  | WhiteStep Int

type InterpreterMemory = (StepState, Memory)
