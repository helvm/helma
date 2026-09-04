{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.AssemblyGenerator
  ( AssemblyProgram (..)
  , Instruction (..)
  , Label
  , generateAssembly
  , renderAssembly
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified Data.IntMap                                 as IM
import qualified Data.Map                                    as M
import qualified Data.Text.Lazy.Builder                      as LText

import           Relude.Extra

type Label = Int

data Instruction
  = ExecCmd Command
  | SetDPCC Course
  | Jump Label
  | BranchDPCC Course Label
  | Exit
  deriving stock (Eq, Show)

data AssemblyProgram
  = AssemblyProgram
      { entryLabel   :: Label
      , entryDPCC    :: Course
      , instructions :: [(Maybe Label, Instruction)]
      }
  deriving stock (Eq, Show)

generateAssembly ∷ Maybe SyntaxGraph → AssemblyProgram
generateAssembly Nothing   = AssemblyProgram 0 initialCourse []
generateAssembly (Just sg) = compileGraph sg

renderAssembly ∷ AssemblyProgram → LText
renderAssembly prog
  | null (instructions prog) = "; Empty Piet Program\nmain:\n    exit\n"
  | otherwise                = LText.toLazyText $ renderAssemblyBuilder prog

-- HELPERS

compileGraph ∷ SyntaxGraph → AssemblyProgram
compileGraph sg = AssemblyProgram (blockIndex entry) (course entry) instrs where
  entry  = sg ^. entryL
  bMap   = sg ^. blockMapL
  instrs = compileBlocks $ IM.toAscList bMap

compileBlocks ∷ [(Int, Block)] → [(Maybe Label, Instruction)]
compileBlocks = foldMap (uncurry compileBlock)

compileBlock ∷ Int → Block → [(Maybe Label, Instruction)]
compileBlock lbl block = attachLabel lbl $ compileTransitions $ M.toAscList $ block ^. transitionsL

attachLabel ∷ Label → [Instruction] → [(Maybe Label, Instruction)]
attachLabel _ []         = []
attachLabel lbl (i : is) = (Just lbl, i) : fmap (Nothing ,) is

compileTransitions ∷ [(Course, Maybe NextBlock)] → [Instruction]
compileTransitions = foldMap (uncurry compileBranch)

compileBranch ∷ Course → Maybe NextBlock → [Instruction]
compileBranch c Nothing   = [BranchDPCC c exitTarget]
compileBranch c (Just nb) = BranchDPCC c targetLbl : handleNextBlock nb where
  targetLbl = blockIndex $ nb ^. targetL

exitTarget ∷ Label
exitTarget = -1

handleNextBlock ∷ NextBlock → [Instruction]
handleNextBlock nb = filterNotNop [ExecCmd cmd, SetDPCC targetCourse] where
  cmd          = nb ^. commandL
  targetCourse = course $ nb ^. targetL

filterNotNop ∷ [Instruction] → [Instruction]
filterNotNop = filter (/= ExecCmd NoOperation)

-- BUILDER

renderAssemblyBuilder ∷ AssemblyProgram → LText.Builder
renderAssemblyBuilder prog =
  "; --- PIET FLAT ASSEMBLY LISTING ---\n"
    <> "; Entry point: label_" <> showBuilder (entryLabel prog) <> "\n"
    <> "; Initial Course: " <> showBuilder (entryDPCC prog) <> "\n\n"
    <> mconcat (renderLabeledInstruction <$> instructions prog)

renderLabeledInstruction ∷ (Maybe Label, Instruction) → LText.Builder
renderLabeledInstruction (Just lbl, inst) = "label_" <> showBuilder lbl <> ":\n" <> renderInstruction inst
renderLabeledInstruction (Nothing, inst)  = renderInstruction inst

renderInstruction ∷ Instruction → LText.Builder
renderInstruction (ExecCmd cmd)      = "    " <> toStringBuilder (showCommand cmd) <> "\n"
renderInstruction (SetDPCC dpcc)     = "    set_dpcc " <> showBuilder dpcc <> "\n"
renderInstruction (Jump target)      = "    jump label_" <> showBuilder target <> "\n"
renderInstruction (BranchDPCC c lbl) = "    branch_dpcc " <> showBuilder c <> " label_" <> showBuilder lbl <> "\n"
renderInstruction Exit               = "    exit\n"

toStringBuilder ∷ ToString a ⇒ a → LText.Builder
toStringBuilder = LText.fromString . toString

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
