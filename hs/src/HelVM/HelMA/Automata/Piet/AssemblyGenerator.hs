{-# LANGUAGE OverloadedStrings #-}

-- | Functions to generate a high-level assembly IR and pretty-printed code from 'SyntaxGraph'.
module HelVM.HelMA.Automata.Piet.AssemblyGenerator
  ( -- * Types
    AssemblyProgram (..)
  , BlockAssembly (..)
  , Instruction (..)
  , Label
    -- * Generation
  , generateAssembly
    -- * Pretty Printing
  , renderAssembly
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course      ( Course )
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified Data.IntMap                                 as IM
import qualified Data.Map                                    as M

-- | Label identifying a basic block in the assembly code.
type Label = Int

-- | High-level instructions representing Piet execution flow.
data Instruction
  = ExecCmd Command
  | SetDPCC Course
  | Jump Label
  | Exit
  deriving stock (Eq, Show)

-- | A single basic block with a label, Course switch-case branches, and fallback transitions.
data BlockAssembly
  = BlockAssembly
      { blockLabel   :: Label
      , dpccBranches :: [(Course, [Instruction])]
      }
  deriving stock (Eq, Show)

-- | The complete compiled Piet assembly program.
data AssemblyProgram
  = AssemblyProgram
      { entryLabel :: Label
      , entryDPCC  :: Course
      , blocks     :: IntMap BlockAssembly
      }
  deriving stock (Eq, Show)

-- | Generate an 'AssemblyProgram' IR from a 'SyntaxGraph'.
generateAssembly ∷ SyntaxGraph → AssemblyProgram
generateAssembly (SyntaxGraph entryEdge bMap) =
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
-- Pretty Printing / Assembly Text Generation
--------------------------------------------------------------------------------

-- | Renders the 'AssemblyProgram' into a readable textual assembly listing.
renderAssembly ∷ AssemblyProgram → Text
renderAssembly (AssemblyProgram _ _ blockMap)
  | IM.null blockMap = "; Empty Piet Program\nmain:\n    exit\n"
renderAssembly prog = unlines $
  [ "; --- PIET ASSEMBLY LISTING ---"
  , "; Entry point: block_" <> show (entryLabel prog)
  , "; Initial Course: " <> show (entryDPCC prog)
  , ""
  ] <> concatMap renderBlock (IM.toList $ blocks prog)

renderBlock ∷ (Label, BlockAssembly) → [Text]
renderBlock (lbl, block) =
  ("block_" <> show lbl <> ":") : concatMap renderBranch (dpccBranches block)
  where
    renderBranch (dpcc, instrs) =
      ("  case_dpcc " <> show dpcc <> ":") : map (\i → "    " <> renderInstruction i) instrs

renderInstruction ∷ Instruction → Text
renderInstruction (ExecCmd cmd)  = renderCommand cmd
renderInstruction (SetDPCC dpcc) = "set_dpcc " <> show dpcc
renderInstruction (Jump target)  = "jump block_" <> show target
renderInstruction Exit           = "exit"

renderCommand ∷ Command → Text
renderCommand NoOperation = "nop"
renderCommand (Push n)    = "push " <> show n
renderCommand Pop         = "pop"
renderCommand Add         = "add"
renderCommand Subtract    = "sub"
renderCommand Multiply    = "mul"
renderCommand Divide      = "div"
renderCommand Mod         = "mod"
renderCommand Not         = "not"
renderCommand Greater     = "greater"
renderCommand Pointer     = "pointer"
renderCommand Switch      = "switch"
renderCommand Duplicate   = "dup"
renderCommand Roll        = "roll"
renderCommand InNumber    = "in_num"
renderCommand InChar      = "in_char"
renderCommand OutNumber   = "out_num"
renderCommand OutChar     = "out_char"
