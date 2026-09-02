module HelVM.HelMA.Automata.Piet.SyntaxVisualizer
  ( syntaxToDOT
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Course
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified Data.IntMap                                 as IM
import qualified Data.Map                                    as M
import qualified Data.Text.Lazy.Builder                      as LText

import           Relude.Extra

syntaxToDOT ∷ Maybe SyntaxGraph → LText
syntaxToDOT Nothing   = "digraph {}"
syntaxToDOT (Just sg) = LText.toLazyText $ "digraph {\n" <> "  rankdir=LR\n" <> "  start [label=\"\" shape=point color=white]\n" <> "  node [label=\"\" shape=circle color=black]\n" <> startEdge (view entryL sg) <> mconcat (blocks $ view blockMapL sg) <> "}"

startEdge ∷ BlockEdge → LText.Builder
startEdge edge = "  start -> " <> showBuilder (view blockIndexL edge) <> " [label=\"" <> toStringBuilder (showCourse $ view courseL edge) <> "\"]\n"

blocks ∷ IntMap Block → [LText.Builder]
blocks = fmap (uncurry processBlock) . IM.toAscList

processBlock ∷ Int → Block → LText.Builder
processBlock from = processCourseList from . M.toAscList . view transitionsL

processCourseList ∷ Int → [(Course, Maybe NextBlock)] → LText.Builder
processCourseList from []                 = emptyBlock from
processCourseList from courseAndNextBlock = nonemptyBlock from courseAndNextBlock

nonemptyBlock ∷ Int → [(Course, Maybe NextBlock)] → LText.Builder
nonemptyBlock from courseAndNextBlock = nodeLine from courseAndNextBlock <> edgeLines from courseAndNextBlock

nodeLine ∷ Int → [(Course, Maybe NextBlock)] → LText.Builder
nodeLine from courseAndNextBlock
  | hasExit courseAndNextBlock = exitEdge from
  | otherwise                  = ""

hasExit ∷ [(Course, Maybe NextBlock)] → Bool
hasExit = any (isNothing . snd)

edgeLines ∷ Int → [(Course, Maybe NextBlock)] → LText.Builder
edgeLines from = foldMap (uncurry $ nextBlockEdge from)

emptyBlock ∷ Int → LText.Builder
emptyBlock from = exitEdge from <> "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"\"]\n"

exitEdge ∷ Int → LText.Builder
exitEdge from = "  exit" <> showBuilder from <> " [label=\"\" shape=point color=white]\n"

nextBlockEdge ∷ Int → Course → Maybe NextBlock → LText.Builder
nextBlockEdge from fromCourse (Just nb) = formatNextBlockEdge from fromCourse (view commandL nb) (view targetL nb)
nextBlockEdge from fromCourse Nothing   = "  " <> showBuilder from <> " -> exit" <> showBuilder from <> " [label=\"" <> toStringBuilder (showCourse fromCourse) <> "\"]\n"

formatNextBlockEdge ∷ Int → Course → Command → BlockEdge → LText.Builder
formatNextBlockEdge from fromCourse cmd targetEdge = "  " <> showBuilder from <> " -> " <> showBuilder (view blockIndexL targetEdge) <> " [label=\"" <> toStringBuilder (showCourse fromCourse) <> ": " <> toStringBuilder (showCommand cmd) <> nextCourseText fromCourse (view courseL targetEdge) <> "\"]\n"

nextCourseText ∷ Course → Course → LText.Builder
nextCourseText fromCourse toCourse
  | toCourse /= fromCourse = " -> " <> toStringBuilder (showCourse toCourse)
  | otherwise              = ""

toStringBuilder ∷ ToString a ⇒ a → LText.Builder
toStringBuilder = LText.fromString . toString

showBuilder ∷ Show a ⇒ a → LText.Builder
showBuilder = LText.fromString . show
