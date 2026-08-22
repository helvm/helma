module HelVM.HelMA.Automaton.Automaton
  ( runAndDumpLogs
  , runAutomat
  , start
  ) where

import           HelVM.HelMA.Automaton.API.AutomatonOptions
import           HelVM.HelMA.Automaton.API.AutoOptions

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.Eff.AutomatonEff
import           HelVM.HelMA.Automaton.Eff.MonadEff

import           HelVM.HelMA.Automaton.Optimizer
import           HelVM.HelMA.Automaton.Symbol
import           HelVM.HelMA.Automaton.Trampoline           as Trampoline

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelMA.Automaton.Combiner
import           HelVM.HelMA.Automaton.Combiner.CPU         as CPU

import qualified HelVM.HelIO.Collections.MapList            as MapList
import qualified HelVM.HelIO.Collections.SList              as SList

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           Control.Monad.Extra

import qualified Data.Sequence                              as Seq

import           Prelude                                    hiding ( swap )

start ∷ AppSafeEff m ⇒ InstructionList → AutomatonOptions → m ()
start il ao = start' (flip optimize il $ optLevelAutoOptions ao) (stackType ao) (ramType ao) (autoOptions ao)

start' ∷ AppSafeEff m ⇒ InstructionList → StackType → RAMType → AutoOptions → m ()
start' il s ListRAMType    = start'' il s []
start' il s SeqRAMType     = start'' il s Seq.empty
start' il s SListRAMType   = start'' il s SList.sListEmpty
start' il s MapListRAMType = start'' il s MapList.mapListEmpty

start'' ∷ (RAutomatonEff Symbol r m) ⇒ InstructionList → StackType → r → AutoOptions → m ()
start'' il ListStackType  = start''' il []
start'' il SeqStackType   = start''' il Seq.empty
start'' il SListStackType = start''' il SList.sListEmpty

start''' ∷ (SRAutomatonEff Symbol s r m) ⇒ InstructionList → s → r → AutoOptions → m ()
start''' il s r p = runAndDumpLogs p (newMemory il s r)

runAndDumpLogs ∷ (SRAutomatonEff Symbol s r m) ⇒ AutoOptions → Memory s r →  m ()
runAndDumpLogs p = logDump (dumpType p) <=< runAutomat (limit p)

runAutomat ∷ (SRAutomatonEff Symbol s r m) ⇒ LimitMaybe → F s r m
runAutomat = trampolineMWithLimit nextState

nextState ∷ (SRAutomatonEff Symbol s r m) ⇒ SF s r m
nextState a = nextStateForInstruction =<< currentInstruction (memoryCM a) where
  nextStateForInstruction i = appendErrorTuple ("Automaton.nextState" , showP a) $ appendErrorTuple ("program:" , printIndexedIL $ toList program) $ appendErrorTuple ("i:" , show i) $ runInstruction i $ incrementIC a where
    program = memoryProgram a
