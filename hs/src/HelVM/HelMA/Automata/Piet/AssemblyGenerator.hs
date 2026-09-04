{-# LANGUAGE OverloadedStrings #-}

module HelVM.HelMA.Automata.Piet.AssemblyGenerator
  ( AssemblyProgram (..)
  , BlockAssembly (..)
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

type Label = Int

data Instruction
  = ExecCmd Command
  | SetDPCC Course
  | Jump Label
  | Exit
  deriving stock (Eq, Show)

data BlockAssembly
  = BlockAssembly
      { blockLabel   :: Label
      , dpccBranches :: [(Course, [Instruction])]
      }
  deriving stock (Eq, Show)

data AssemblyProgram
  = AssemblyProgram
      { entryLabel :: Label
      , entryDPCC  :: Course
      , blocks     :: IntMap BlockAssembly
      }
  deriving stock (Eq, Show)

generateAssembly ∷ Maybe SyntaxGraph → AssemblyProgram
generateAssembly Nothing = AssemblyProgram 0 initialCourse IM.empty
generateAssembly (Just (SyntaxGraph entryEdge bMap)) =
  AssemblyProgram
    { entryLabel = blockIndex entryEdge
    , entryDPCC  = course entryEdge
    , blocks     = IM.mapWithKey compileBlock bMap
    }

compileBlock ∷ Label → Block → BlockAssembly
compileBlock lbl block =
  BlockAssembly
    { blockLabel   = lbl
    , dpccBranches = map (uncurry compileBranch) (M.toList $ transitions block)
    }

compileBranch ∷ Course → Maybe NextBlock → (Course, [Instruction])
compileBranch currentCourse maybeNextBlock = (currentCourse, handleNextBlock maybeNextBlock)

handleNextBlock ∷ Maybe NextBlock → [Instruction]
handleNextBlock Nothing = [Exit]
handleNextBlock (Just (NextBlock cmd targetEdge)) =
  filterNotNop [ExecCmd cmd, SetDPCC (course targetEdge), Jump (blockIndex targetEdge)]

filterNotNop ∷ [Instruction] → [Instruction]
filterNotNop = filter (/= ExecCmd NoOperation)

--------------------------------------------------------------------------------
-- Pretty Printing / Assembly Text Generation (Builder Pattern)
--------------------------------------------------------------------------------

renderAssembly ∷ AssemblyProgram → LText
renderAssembly prog
  | IM.null (blocks prog) = "; Empty Piet Program\nmain:\n    exit\n"
  | otherwise            = LText.toLazyText $ renderAssemblyBuilder prog

renderAssemblyBuilder ∷ AssemblyProgram → LText.Builder
renderAssemblyBuilder prog =
  "; --- PIET ASSEMBLY LISTING ---\n"
    <> "; Entry point: block_" <> showBuilder (entryLabel prog) <> "\n"
    <> "; Initial Course: " <> showBuilder (entryDPCC prog) <> "\n\n"
    <> mconcat (renderBlock <$> IM.toList (blocks prog))

renderBlock ∷ (Label, BlockAssembly) → LText.Builder
renderBlock (lbl, block) =
  "block_" <> showBuilder lbl <> ":\n"
    <> mconcat (renderBranch <$> dpccBranches block)
  where
    renderBranch (dpcc, instrs) =
      "  case_dpcc " <> showBuilder dpcc <> ":\n"
        <> mconcat (renderInstruction <$> instrs)

renderInstruction ∷ Instruction → LText.Builder
renderInstruction (ExecCmd cmd)  = "    " <> toStringBuilder (renderCommand cmd) <> "\n"
renderInstruction (SetDPCC dpcc) = "    set_dpcc " <> showBuilder dpcc <> "\n"
renderInstruction (Jump target)  = "    jump block_" <> showBuilder target <> "\n"
renderInstruction Exit           = "    exit\n"

toStringBuilder ∷ ToString a ⇒ a → LText.Builder
toStringBuilder = LText.fromString . toString

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
