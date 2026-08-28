module HelVM.HelMA.Automata.Piet.LLVM.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Syntax

import qualified Data.IntMap                           as IM
import qualified Data.Map                              as M
import qualified Data.Text.Lazy.Builder                as LText

syntaxToDOT ∷ SyntaxGraph → LText
syntaxToDOT EmptySyntaxGraph = "digraph {}"
syntaxToDOT (SyntaxGraph blockIndex dpcc blockMap) =
  LText.toLazyText $  "digraph {\n"
             <> "  rankdir=LR\n"
             <> "  start [label=\"\" shape=point color=white]\n"
             <> "  node [label=\"\" shape=circle color=black]\n"
             <> startEdge blockIndex dpcc
             <> mconcat (blocks blockMap)
             <> "}"

startEdge ∷ Int → DPCC → LText.Builder
startEdge blockIndex dpcc =
  "  start -> " <> showBuilder blockIndex <> " [label=\"" <> fromString (showDPCC dpcc) <> "\"]\n"

blocks ∷ IntMap Block → [LText.Builder]
blocks blockMap = do
  (from, block) <- IM.toAscList blockMap
  processBlock from (M.toAscList $ nextBlockTable block)
  where
    processBlock from []               = pure $ emptyBlock from
    processBlock from dpccAndNextBlock = pure $ nonemptyBlock from dpccAndNextBlock

nonemptyBlock ∷ Int → [(DPCC, NextBlock)] → LText.Builder
nonemptyBlock from dpccAndNextBlock = nodeLine <> edgeLines
  where
    hasExit = any ((== ExitProgram) . snd) dpccAndNextBlock
    nodeLine
      | hasExit   = exitEdge from
      | otherwise = ""
    edgeLines = foldMap (uncurry $ nextBlockEdge from) dpccAndNextBlock

emptyBlock ∷ Int → LText.Builder
emptyBlock from = exitEdge from <> "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"\"]\n"

exitEdge ∷ Int → LText.Builder
exitEdge from = "  exit" <> showBuilder from <> " [label=\"\" shape=point color=white]\n"

nextBlockEdge ∷ Int → DPCC → NextBlock → LText.Builder
nextBlockEdge from fromDPCC (NextBlock command toDPCC nextBlockIndex) =
  "  " <> showBuilder from <> " -> " <> showBuilder nextBlockIndex
       <> " [label=\"" <> fromString (showDPCC fromDPCC) <> ": " <> fromString (showCommand command) <> nextDPCCText toDPCC <> "\"]\n"
  where
    nextDPCCText toDPCC'
      | toDPCC' /= fromDPCC = " -> " <> fromString (showDPCC toDPCC')
      | otherwise          = ""

nextBlockEdge from fromDPCC ExitProgram =
  "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"" <> fromString (showDPCC fromDPCC) <> "\"]\n"

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
