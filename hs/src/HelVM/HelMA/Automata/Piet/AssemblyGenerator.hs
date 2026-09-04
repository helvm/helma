{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.AssemblyGenerator
  ( AssemblyProgram (..)
  , BlockAssembly (..)
  , BranchAssembly (..)
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
  | StoreDPCC Course
  | Jump Label
  | Exit
  deriving stock (Eq, Show)

data BranchAssembly
  = BranchAssembly
      { branchCourses :: [Course]
      , branchInstrs  :: [Instruction]
      }
  deriving stock (Eq, Show)

data BlockAssembly
  = BlockAssembly
      { blockLabel :: Label
      , branches   :: [BranchAssembly]
      }
  deriving stock (Eq, Show)

data AssemblyProgram
  = AssemblyProgram
      { entryLabel :: Label
      , entryDPCC  :: Course
      , blocks     :: [BlockAssembly]
      }
  deriving stock (Eq, Show)

generateAssembly ∷ Maybe SyntaxGraph → AssemblyProgram
generateAssembly Nothing   = AssemblyProgram 0 initialCourse []
generateAssembly (Just sg) = compileGraph sg

renderAssembly ∷ AssemblyProgram → LText
renderAssembly prog
  | null (blocks prog) = "; Empty Piet Program\nmain:\n    exit\n"
  | otherwise          = LText.toLazyText $ renderAssemblyBuilder prog

-- HELPERS

compileGraph ∷ SyntaxGraph → AssemblyProgram
compileGraph sg = AssemblyProgram (blockIndex entry) (course entry) blockList where
  entry     = sg ^. entryL
  bMap      = sg ^. blockMapL
  blockList = uncurry compileBlock <$> IM.toAscList bMap

compileBlock ∷ Label → Block → BlockAssembly
compileBlock lbl block = BlockAssembly lbl $ compileBranches $ M.toAscList $ block ^. transitionsL

compileBranches ∷ [(Course, Maybe NextBlock)] → [BranchAssembly]
compileBranches transitions = uncurry (compileBranch backwardTable) <$> transitions where
  backwardTable = buildBackwardTable $ fst <$> transitions

compileBranch ∷ Map Course [Course] → Course → Maybe NextBlock → BranchAssembly
compileBranch bwdTable c maybeNext = BranchAssembly currentCourses $ handleNextBlock currentCourses maybeNext where
  currentCourses = fromMaybe [c] $ M.lookup c bwdTable

handleNextBlock ∷ [Course] → Maybe NextBlock → [Instruction]
handleNextBlock _ Nothing   = [Exit]
handleNextBlock currentCourses (Just nb) = filterNotNop $ storeInstr ++ [ExecCmd cmd, Jump targetLbl] where
  cmd          = nb ^. commandL
  targetEdge   = nb ^. targetL
  targetCourse = course targetEdge
  targetLbl    = blockIndex targetEdge
  storeInstr   = [StoreDPCC targetCourse | currentCourses /= [targetCourse]]

filterNotNop ∷ [Instruction] → [Instruction]
filterNotNop = filter (/= ExecCmd NoOperation)

buildBackwardTable ∷ [Course] → Map Course [Course]
buildBackwardTable courses = M.fromList $ fmap (\c -> (c, [c])) courses

-- BUILDER

renderAssemblyBuilder ∷ AssemblyProgram → LText.Builder
renderAssemblyBuilder prog =
  "; --- PIET SWITCH-BASED ASSEMBLY LISTING ---\n"
    <> "; Entry point: block_" <> showBuilder (entryLabel prog) <> "\n"
    <> "; Initial Course: " <> showBuilder (entryDPCC prog) <> "\n\n"
    <> mconcat (renderBlock <$> blocks prog)

renderBlock ∷ BlockAssembly → LText.Builder
renderBlock block =
  "block_" <> showBuilder (blockLabel block) <> ":\n"
    <> "    switch_dpcc\n"
    <> mconcat (renderBranch <$> branches block)

renderBranch ∷ BranchAssembly → LText.Builder
renderBranch branch =
  "  case " <> renderCourses (branchCourses branch) <> ":\n"
    <> mconcat (renderInstruction <$> branchInstrs branch)

renderCourses ∷ [Course] → LText.Builder
renderCourses cs = mconcat $ intersperse ", " (showBuilder <$> cs)

renderInstruction ∷ Instruction → LText.Builder
renderInstruction (ExecCmd cmd)    = "    " <> toStringBuilder (showCommand cmd) <> "\n"
renderInstruction (StoreDPCC dpcc) = "    store_dpcc " <> showBuilder dpcc <> "\n"
renderInstruction (Jump target)    = "    jump block_" <> showBuilder target <> "\n"
renderInstruction Exit             = "    exit\n"

toStringBuilder ∷ ToString a ⇒ a → LText.Builder
toStringBuilder = LText.fromString . toString

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
