{-# LANGUAGE TemplateHaskell #-}

module HelVM.HelMA.Automata.Piet.Automaton.StepState
  ( memory
  , start
  , stepState
  ) where

import           HelVM.HelMA.Automata.Piet.Combiner

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.InstructionCounter
import           HelVM.HelMA.Automata.Piet.Types.Label
import           HelVM.HelMA.Automata.Piet.Types.Memory
import           HelVM.HelMA.Automata.Piet.Types.Program            as Program

import           HelVM.HelMA.Automaton.Eff.MonadEff
import           HelVM.HelMA.Automaton.Trampoline                   as Trampoline

import           HelVM.HelIO.Control.Safe

import           Lens.Micro.Platform

-- Types & Lenses

data StepState
  = ChromaticStep (Maybe PreviousColor)
  | WhiteStep Int

data AutomatonMemory
  = AutomatonMemory
      { _stepState :: !StepState
      , _memory    :: !Memory
      }

makeLenses ''AutomatonMemory

type ChromaticMaybeMemory = (Maybe PreviousColor, Memory)
type ChromaticMemory      = (PreviousColor, Memory)

-- Main interpreter entry point

initialState ∷ Program → AutomatonMemory
initialState prog = AutomatonMemory
  { _stepState = ChromaticStep Nothing
  , _memory    = initialMemory prog
  }

start ∷ AppSafeEff m ⇒ Program → m ()
start = Trampoline.trampolineM interpretStep . initialState

interpretStep ∷ AppSafeEff m ⇒ AutomatonMemory → m (Either () AutomatonMemory)
interpretStep (AutomatonMemory (ChromaticStep prev) mem) = stepChromatic (prev, mem)
interpretStep (AutomatonMemory (WhiteStep limit)    mem) = pure $ stepWhite limit mem

-- Step handlers

stepChromatic ∷ AppSafeEff m ⇒ ChromaticMaybeMemory → m (Either () AutomatonMemory)
stepChromatic cmMem@(_, mem) = evalPixel (currentPixel mem) cmMem

stepWhite ∷ Int → Memory → Either () AutomatonMemory
stepWhite limit mem
  | limit <= 0 = Trampoline.break ()
  | otherwise  = Trampoline.continue $ checkWhitePixel (currentPixel mem) limit mem

-- Pixel handlers

evalPixel ∷ AppSafeEff m ⇒ Color → ChromaticMaybeMemory → m (Either () AutomatonMemory)
evalPixel (Chromatic color) cmMem    = evalChromaticPixel color cmMem
evalPixel White             (_, mem) = pure $ Trampoline.continue $ evalWhitePixel mem
evalPixel Black             _        = liftError "Entered black block, terminate"

evalChromaticPixel ∷ AppSafeEff m ⇒ ChromaticColor → ChromaticMaybeMemory → m (Either () AutomatonMemory)
evalChromaticPixel color (previous, mem) = makeNext <$> applyPreviousColor previous color mem where
  makeNext mem' = handleNext (nonBlackSuccMemory mem' mStats) ((color, getLabelSize mStats), mem')
  mStats        = getMaskInfo mem

evalWhitePixel ∷ Memory → AutomatonMemory
evalWhitePixel mem = AutomatonMemory
  { _stepState = WhiteStep (8 * getLabelSize (getMaskInfo mem))
  , _memory    = mem
  }

checkWhitePixel ∷ Color → Int → Memory → AutomatonMemory
checkWhitePixel White limit mem = AutomatonMemory (WhiteStep (limit - 1)) (stepWhitePixel mem)
checkWhitePixel _     _     mem = AutomatonMemory (ChromaticStep Nothing) mem

-- Helper functions

handleNext ∷ Maybe InstructionCounter → ChromaticMemory → Either () AutomatonMemory
handleNext (Just ic) (color, mem) = Trampoline.continue $ handleNextSuccess (color, setInstructionCounter ic mem)
handleNext Nothing   _            = Trampoline.break ()

handleNextSuccess ∷ ChromaticMemory → AutomatonMemory
handleNextSuccess (prevColor, mem) = AutomatonMemory (ChromaticStep (Just prevColor)) mem
