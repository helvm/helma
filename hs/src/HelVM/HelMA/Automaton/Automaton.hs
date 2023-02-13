module HelVM.HelMA.Automaton.Automaton (
  start,
  runAndDumpLogs,
  run,
) where

import           HelVM.HelMA.Automaton.API.AutoOptions
import           HelVM.HelMA.Automaton.API.AutomatonOptions

import           HelVM.HelMA.Automaton.Instruction

import           HelVM.HelMA.Automaton.IO.AutomatonIO
import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Loop                 as Loop
import           HelVM.HelMA.Automaton.Optimizer
import           HelVM.HelMA.Automaton.Symbol

import           HelVM.HelMA.Automaton.Types.DumpType
import           HelVM.HelMA.Automaton.Types.RAMType
import           HelVM.HelMA.Automaton.Types.StackType

import           HelVM.HelMA.Automaton.Combiner
import           HelVM.HelMA.Automaton.Combiner.CPU         as CPU

import           HelVM.HelIO.Containers.LLIndexSafe

import qualified HelVM.HelIO.Collections.MapList            as MapList
import qualified HelVM.HelIO.Collections.SList              as SList

import           Control.Applicative.Tools
import           Control.Monad.Extra

import qualified Data.Sequence                              as Seq
import           Data.Vector                                as Vector

import           Prelude                                    hiding (swap)

start :: BIO m => InstructionList -> AutomatonOptions -> m ()
start il ao = start' (flip optimize il $ optLevelAutoOptions ao) (stackType ao) (ramType ao) (autoOptions ao)

start' :: BIO m => InstructionList -> StackType -> RAMType -> AutoOptions -> m ()
start' il s ListRAMType    = start'' il s []
start' il s SeqRAMType     = start'' il s Seq.empty
start' il s SListRAMType   = start'' il s SList.sListEmpty
start' il s MapListRAMType = start'' il s MapList.mapListEmpty

start'' :: (RAutomatonIO Symbol r m) => InstructionList -> StackType -> r -> AutoOptions -> m ()
start'' il ListStackType  = start''' il []
start'' il SeqStackType   = start''' il Seq.empty
start'' il SListStackType = start''' il SList.sListEmpty

start''' :: (SRAutomatonIO Symbol s r m) => InstructionList -> s -> r -> AutoOptions -> m ()
start''' il s r p = runAndDumpLogs p (newMemory il s r)

runAndDumpLogs :: (SRAutomatonIO Symbol s r m) => AutoOptions -> Memory s r ->  m ()
runAndDumpLogs p = logDump (dumpType p) <=< run (compileFlag p) (limit p)

run :: (SRAutomatonIO Symbol s r m) => Bool -> LimitMaybe -> F s r m
run False = runI
run True  = runA --FIXME Remove it because it does not work

----

runA :: (SRAutomatonIO Symbol s r m) => LimitMaybe -> F s r m
runA l a = loopMWithLimit (nextStateA $ compileA a) l  a

compileA :: (SRAutomatonIO Symbol s r m) => Memory s r -> Vector (SF s r m)
compileA = runInstruction <.> memoryProgram

nextStateA :: (SRAutomatonIO Symbol s r m) => Vector (SF s r m) -> SF s r m
nextStateA fv a = flip id (incrementIC a) =<< indexSafe fv (memoryProgramCounter a)

----

runI :: (SRAutomatonIO Symbol s r m) => LimitMaybe -> F s r m
runI = loopMWithLimit nextStateI

nextStateI :: (SRAutomatonIO Symbol s r m) => SF s r m
nextStateI a = nextStateForInstruction =<< currentInstruction (memoryCM a) where
  nextStateForInstruction i = runInstruction i $ incrementIC a
