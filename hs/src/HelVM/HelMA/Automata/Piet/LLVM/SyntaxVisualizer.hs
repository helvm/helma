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
nonemptyBlock from courseAndNextBlock = nodeLine from courseAndNextBlock <> edgeLines from courseAndNextBlock

nodeLine ∷ Int → [(Course, NextBlockMaybe)] → LText.Builder
nodeLine from courseAndNextBlock
  | hasExit courseAndNextBlock = exitEdge from
  | otherwise                  = ""

hasExit ∷ [(Course, NextBlockMaybe)] → Bool
hasExit = any (isNothing . snd)

edgeLines ∷ Int → [(Course, NextBlockMaybe)] → LText.Builder
edgeLines from = foldMap (uncurry $ nextBlockEdge from)

emptyBlock ∷ Int → LText.Builder
emptyBlock from = exitEdge from <> "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"\"]\n"

exitEdge ∷ Int → LText.Builder
exitEdge from = "  exit" <> showBuilder from <> " [label=\"\" shape=point color=white]\n"

nextBlockEdge ∷ Int → Course → NextBlockMaybe → LText.Builder
nextBlockEdge from fromCourse (Just (NextBlock command toCourse nextBlockIndex)) = "  " <> showBuilder from <> " -> " <> showBuilder nextBlockIndex <> " [label=\"" <> fromString (showCourse fromCourse) <> ": " <> fromString (showCommand command) <> nextCourseText fromCourse toCourse <> "\"]\n"
nextBlockEdge from fromCourse Nothing                                           = "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"" <> fromString (showCourse fromCourse) <> "\"]\n"

nextCourseText ∷ Course → Course → LText.Builder
nextCourseText fromCourse toCourse
  | toCourse /= fromCourse = " -> " <> fromString (showCourse toCourse)
  | otherwise              = ""

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
