{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.InstructionCompiler
  ( compileToIL
  ) where

import qualified HelVM.HelMA.Automata.Piet.AssemblyGenerator            as AG

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import qualified HelVM.HelMA.Automata.Piet.Types.Command                as Piet
import           HelVM.HelMA.Automata.Piet.Types.Course

import qualified Data.Text                                              as T
import           HelVM.HelIO.Collections.SList                          ( sListFromList )

dpccRamAddress ∷ Integer
dpccRamAddress = 0

compileToIL ∷ AG.AssemblyProgram → InstructionList
compileToIL prog = initDPCC (AG.entryDPCC prog) <> foldMap compileBlock (AG.blocks prog) <> [End]

initDPCC ∷ Course → InstructionList
initDPCC c =
  [ ISM (SPure (Cons dpccRamAddress))
  , ISM (SPure (Cons (courseToVal c)))
  , ILS Store
  ]

compileBlock ∷ AG.BlockAssembly → InstructionList
compileBlock block = blockMark : foldMap (compileBranch blockLbl) (AG.branches block) where
  blockLbl  = AG.blockLabel block
  blockMark = ICF (Mark (MArtificial (showBlockLabel blockLbl)))

compileBranch ∷ AG.Label → AG.BranchAssembly → InstructionList
compileBranch blockLbl branch = checkCourses (AG.branchCourses branch) nextLabel <> branchCode <> [jumpNext] where
  nextLabel  = showBranchLabel blockLbl (AG.branchCourses branch)
  jumpNext   = ICF (Mark (MArtificial nextLabel))
  branchCode = foldMap compileInstruction (AG.branchInstrs branch)

checkCourses ∷ [Course] → CFInstructionLabel → InstructionList
checkCourses [] _ = []
checkCourses (c : cs) targetLabel =
  loadDPCC
    <> [ ISM (SPure (Cons (courseToVal c)))
       , ISM (SPure (Binary Sub))
       , ICF (Branch (BArtificial targetLabel) EZ)
       ]
    <> checkCourses cs targetLabel

loadDPCC ∷ InstructionList
loadDPCC =
  [ ISM (SPure (Cons dpccRamAddress))
  , ILS Load
  ]

compileInstruction ∷ AG.Instruction → InstructionList
compileInstruction (AG.ExecCmd cmd) = compileCommand cmd
compileInstruction (AG.StoreDPCC c) = initDPCC c
compileInstruction (AG.Jump lbl)    = [ICF (Labeled (LArtificial (showBlockLabel lbl)) Jump)]
compileInstruction AG.Exit          = [End]

compileCommand ∷ Piet.Command → InstructionList
compileCommand Piet.NoOperation = []
compileCommand (Piet.Push n)    = [ISM (SPure (Cons (toInteger n)))]
compileCommand Piet.Pop         = [ISM (SPure Discard)]
compileCommand Piet.Add         = [ISM (SPure (Binary Add))]
compileCommand Piet.Subtract    = [ISM (SPure (Binary Sub))]
compileCommand Piet.Multiply    = [ISM (SPure (Binary Mul))]
compileCommand Piet.Divide      = [ISM (SPure (Binary Div))]
compileCommand Piet.Mod         = [ISM (SPure (Binary Mod))]
compileCommand Piet.Not         = [ISM (SPure (Unary LNot))]
compileCommand Piet.Greater     = [ISM (SPure (Binary LGT))]
compileCommand Piet.Duplicate   = [ISM (SPure (Indexed (IImmediate 0) Copy))]
compileCommand Piet.InNumber    = [ILS (MIO InputDec)]
compileCommand Piet.InChar      = [ILS (MIO InputChar)]
compileCommand Piet.OutNumber   = [ILS (MIO OutputDec)]
compileCommand Piet.OutChar     = [ILS (MIO OutputChar)]
compileCommand Piet.Pointer     = mutateDP 4
compileCommand Piet.Switch      = mutateCC 2
compileCommand Piet.Roll        = [ISM (SPure Halibut)]

mutateDP ∷ Integer → InstructionList
mutateDP modVal =
  loadDPCC
    <> [ ISM (SPure (Cons 2))
       , ISM (SPure (Binary Div))
       , ISM (SPure (Binary Add))
       , ISM (SPure (Cons modVal))
       , ISM (SPure (Binary Mod))
       , ISM (SPure (Cons 2))
       , ISM (SPure (Binary Mul))
       ]
    <> loadDPCC
    <> [ ISM (SPure (Cons 2))
       , ISM (SPure (Binary Mod))
       , ISM (SPure (Binary Add))
       , ISM (SPure (Cons dpccRamAddress))
       , ISM (SPure Halibut)
       , ILS Store
       ]

mutateCC ∷ Integer → InstructionList
mutateCC modVal =
  loadDPCC
    <> [ ISM (SPure (Cons 2))
       , ISM (SPure (Binary Mod))
       , ISM (SPure (Binary Add))
       , ISM (SPure (Cons modVal))
       , ISM (SPure (Binary Mod))
       ]
    <> loadDPCC
    <> [ ISM (SPure (Cons 2))
       , ISM (SPure (Binary Div))
       , ISM (SPure (Cons 2))
       , ISM (SPure (Binary Mul))
       , ISM (SPure (Binary Add))
       , ISM (SPure (Cons dpccRamAddress))
       , ISM (SPure Halibut)
       , ILS Store
       ]

type CFInstructionLabel = HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction.Label

courseToVal ∷ Course → Integer
courseToVal (Course dp cc) = toInteger (fromEnum dp * 2 + fromEnum cc)

showBlockLabel ∷ AG.Label → CFInstructionLabel
showBlockLabel lbl = sListFromList $ toString ("block_" <> show lbl ∷ Text)

showBranchLabel ∷ AG.Label → [Course] → CFInstructionLabel
showBranchLabel lbl cs = sListFromList $ toString ("branch_" <> show lbl <> "_" <> T.intercalate "_" (toText . showCourse <$> cs))
