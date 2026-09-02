module HelVM.HelMA.Automata.Piet.LLVM.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course

import qualified Data.IntMap                                 as IM
import qualified Data.Map                                    as M
import qualified Data.Text.Lazy.Builder                      as LText

syntaxToDOT ∷ SyntaxGraphMaybe → LText
syntaxToDOT Nothing = "digraph {}"
syntaxToDOT (Just (SyntaxGraph blockIndex course blockMap)) =
  LText.toLazyText $  "digraph {\n"
             <> "  rankdir=LR\n"
             <> "  start [label=\"\" shape=point color=white]\n"
             <> "  node [label=\"\" shape=circle color=black]\n"
             <> startEdge blockIndex course
             <> mconcat (blocks blockMap)
             <> "}"

startEdge ∷ Int → Course → LText.Builder
startEdge blockIndex course =
  "  start -> " <> showBuilder blockIndex <> " [label=\"" <> fromString (showCourse course) <> "\"]\n"

blocks ∷ IntMap Block → [LText.Builder]
blocks = fmap (uncurry processBlock) . IM.toAscList

processBlock ∷ Int → Block → LText.Builder
processBlock from block = processCourseList from (M.toAscList $ nextBlockTable block)

processCourseList ∷ Int → [(Course, NextBlockMaybe)] → LText.Builder
processCourseList from []                 = emptyBlock from
processCourseList from courseAndNextBlock = nonemptyBlock from courseAndNextBlock

nonemptyBlock ∷ Int → [(Course, NextBlockMaybe)] → LText.Builder
nonemptyBlock from courseAndNextBlock = nodeLine <> edgeLines
  where
    hasExit = any ((== Nothing) . snd) courseAndNextBlock
    nodeLine
      | hasExit   = exitEdge from
      | otherwise = ""
    edgeLines = foldMap (uncurry $ nextBlockEdge from) courseAndNextBlock

emptyBlock ∷ Int → LText.Builder
emptyBlock from = exitEdge from <> "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"\"]\n"

exitEdge ∷ Int → LText.Builder
exitEdge from = "  exit" <> showBuilder from <> " [label=\"\" shape=point color=white]\n"

nextBlockEdge ∷ Int → Course → NextBlockMaybe → LText.Builder
nextBlockEdge from fromCourse (Just (NextBlock command toCourse nextBlockIndex)) =
  "  " <> showBuilder from <> " -> " <> showBuilder nextBlockIndex
       <> " [label=\"" <> fromString (showCourse fromCourse) <> ": " <> fromString (showCommand command) <> nextCourseText toCourse <> "\"]\n"
  where
    nextCourseText toCourse'
      | toCourse' /= fromCourse = " -> " <> fromString (showCourse toCourse')
      | otherwise          = ""

nextBlockEdge from fromCourse Nothing =
  "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"" <> fromString (showCourse fromCourse) <> "\"]\n"

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
