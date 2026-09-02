{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.LLVM.SyntaxVisualizerSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxVisualizer

import           HelVM.HelMA.Automata.Piet.Types.Command

import qualified Data.IntMap                                     as IM
import qualified Data.Map                                        as M

import           Test.Hspec
import           Text.InterpolatedString.Perl6

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "syntaxToDOT" $
    forM_
      [ ("emptyGraph", EmptySyntaxGraph, "digraph {}")
      , ("smallestGraph", smallestGraph, smallestDOT)
      , ("stuckGraph", stuckGraph, stuckDOT)
      , ("complexGraph", complexGraph, complexDOT)
      ] $ \(name, graph, dot) ->
        context ("when given " ++ name) $
          it "convert a syntax graph to a DOT script" $ syntaxToDOT graph `shouldBe` dot

smallestGraph ∷ SyntaxGraphMaybe
smallestGraph = SyntaxGraphJust 999 dr $ one (999 , Block M.empty)

smallestDOT ∷ LText
smallestDOT = [q|digraph {
  rankdir=LR
  start [label="" shape=point color=white]
  node [label="" shape=circle color=black]
  start -> 999 [label="dr"]
  exit999 [label="" shape=point color=white]
  999 -> exit999 [label=""]
}|]

stuckGraph ∷ SyntaxGraphMaybe
stuckGraph = SyntaxGraphJust 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, NextBlockJust (Push 1) rl 1)
                         , (rr, NextBlockJust (Push 1) rr 1)
                         , (dl, NextBlockJust NoOperation ul 0)
                         , (dr, NextBlockJust NoOperation ur 0)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, ExitProgram)
                         , (rr, ExitProgram)
                         , (dl, NextBlockJust NoOperation ul 0)
                         , (dr, NextBlockJust NoOperation ur 0)
                         , (ll, NextBlockJust Pop ll 0)
                         , (lr, NextBlockJust Pop lr 0)
                         ]
    )
  ]

stuckDOT ∷ LText
stuckDOT = [q|digraph {
  rankdir=LR
  start [label="" shape=point color=white]
  node [label="" shape=circle color=black]
  start -> 0 [label="rl"]
  0 -> 1 [label="rl: push 1"]
  0 -> 1 [label="rr: push 1"]
  0 -> 0 [label="dl: noop -> ul"]
  0 -> 0 [label="dr: noop -> ur"]
  exit1 [label="" shape=point color=white]
  1 -> exit1 [label="rl"]
  1 -> exit1 [label="rr"]
  1 -> 0 [label="dl: noop -> ul"]
  1 -> 0 [label="dr: noop -> ur"]
  1 -> 0 [label="ll: pop"]
  1 -> 0 [label="lr: pop"]
}|]

complexGraph ∷ SyntaxGraphMaybe
complexGraph = SyntaxGraphJust 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, NextBlockJust Pop rl 1)
                         , (rr, NextBlockJust Pop rr 1)
                         , (dl, NextBlockJust Pop dl 1)
                         , (dr, NextBlockJust (Push 5) dr 6)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, NextBlockJust NoOperation rl 7)
                         , (rr, NextBlockJust NoOperation rr 7)
                         , (dl, NextBlockJust Divide dl 9)
                         , (dr, NextBlockJust Divide dr 9)
                         , (ll, NextBlockJust Pop ll 6)
                         , (lr, NextBlockJust Pop lr 6)
                         ]
    )
  , ( 2
    , Block $ M.fromList [ (rl, NextBlockJust NoOperation rl 4)
                         , (rr, NextBlockJust NoOperation rr 4)
                         , (dl, NextBlockJust NoOperation dl 9)
                         , (dr, NextBlockJust Roll dr 1)
                         , (ll, NextBlockJust Roll ll 1)
                         , (lr, NextBlockJust Roll lr 1)
                         ]
    )
  , ( 4
    , Block $ M.fromList [ (rl, NextBlockJust OutChar rl 5)
                         , (rr, NextBlockJust OutChar rr 5)
                         , (dl, NextBlockJust Subtract dl 7)
                         , (dr, NextBlockJust NoOperation dr 7)
                         , (ll, NextBlockJust NoOperation ll 2)
                         , (lr, NextBlockJust NoOperation lr 2)
                         ]
    )
  , ( 5
    , Block $ M.fromList [ (dr, NextBlockJust Not dr 7)
                         , (ll, NextBlockJust Subtract ll 4)
                         , (lr, NextBlockJust Subtract lr 4)
                         ]
    )
  , ( 6
    , Block $ M.fromList [ (rl, NextBlockJust Mod rl 9)
                         , (rr, NextBlockJust Mod rr 9)
                         , (dl, NextBlockJust Mod dl 9)
                         , (dr, NextBlockJust InChar dr 12)
                         , (ul, NextBlockJust Pop ul 0)
                         , (ur, NextBlockJust Pop ur 0)
                         ]
    )
  , ( 7
    , Block $ M.fromList [ (ll, NextBlockJust NoOperation ll 9)
                         , (lr, NextBlockJust NoOperation lr 9)
                         , (ul, NextBlockJust OutChar ul 4)
                         , (ur, NextBlockJust Roll ur 5)
                         ]
    )
  , ( 9
    , Block $ M.fromList [ (dl, NextBlockJust (Push 16) dl 17)
                         , (dr, NextBlockJust (Push 16) dr 17)
                         , (ll, NextBlockJust Switch ll 12)
                         , (lr, NextBlockJust Switch lr 12)
                         , (ul, NextBlockJust Duplicate ul 1)
                         , (ur, NextBlockJust Duplicate ur 1)
                         ]
    )
  , ( 12
    , Block $ M.fromList [ (rl, NextBlockJust Pointer rl 9)
                         , (rr, NextBlockJust Pointer rr 9)
                         , (dl, NextBlockJust NoOperation dl 23)
                         , (dr, NextBlockJust NoOperation ll 22)
                         , (ul, NextBlockJust Add ul 6)
                         , (ur, NextBlockJust Add ur 6)
                         ]
    )
  , ( 15
    , Block $ M.fromList [ (dl, NextBlockJust Duplicate dl 18)
                         , (dr, NextBlockJust Duplicate dr 18)
                         , (ll, NextBlockJust Roll ll 9)
                         , (lr, NextBlockJust Roll lr 9)
                         , (ul, NextBlockJust Roll ul 9)
                         , (ur, NextBlockJust Roll ur 9)
                         ]
    )
  , ( 17
    , Block $ M.fromList [ (rl, NextBlockJust (Push 1) rl 18)
                         , (rr, NextBlockJust (Push 1) rr 18)
                         , (dl, NextBlockJust NoOperation lr 23)
                         , (dr, NextBlockJust NoOperation ll 23)
                         , (ll, NextBlockJust NoOperation ur 12)
                         , (lr, NextBlockJust NoOperation ul 12)
                         , (ul, NextBlockJust Pop ul 9)
                         , (ur, NextBlockJust Pop ur 9)
                         ]
    )
  , ( 18
    , Block $ M.fromList [ (dl, NextBlockJust Divide dl 25)
                         , (dr, NextBlockJust Divide dr 25)
                         , (ll, NextBlockJust Pop ll 17)
                         , (lr, NextBlockJust Pop lr 17)
                         , (ul, NextBlockJust Divide ul 15)
                         ]
    )
  , ( 22
    , Block $ M.fromList [ (rl, NextBlockJust NoOperation rl 23)
                         , (rr, NextBlockJust NoOperation rr 23)
                         , (ll, NextBlockJust NoOperation ur 12)
                         , (lr, NextBlockJust NoOperation ul 12)
                         , (ul, NextBlockJust NoOperation ul 12)
                         , (ur, NextBlockJust NoOperation ur 12)
                         ]
    )
  , ( 23
    , Block $ M.fromList [ (rl, NextBlockJust NoOperation rl 25)
                         , (rr, NextBlockJust NoOperation rr 25)
                         , (ll, NextBlockJust NoOperation ll 22)
                         , (lr, NextBlockJust NoOperation lr 22)
                         , (ul, NextBlockJust NoOperation ul 12)
                         , (ur, NextBlockJust NoOperation ur 12)
                         ]
    )
  , ( 25
    , Block $ M.fromList [ (rl, NextBlockJust NoOperation ll 25)
                         , (rr, NextBlockJust NoOperation lr 25)
                         , (ll, NextBlockJust NoOperation ll 23)
                         , (lr, NextBlockJust NoOperation lr 23)
                         , (ul, NextBlockJust Duplicate ul 18)
                         , (ur, NextBlockJust Duplicate ur 18)
                         ]
    )
  ]

complexDOT ∷ LText
complexDOT = [q|digraph {
  rankdir=LR
  start [label="" shape=point color=white]
  node [label="" shape=circle color=black]
  start -> 0 [label="rl"]
  0 -> 1 [label="rl: pop"]
  0 -> 1 [label="rr: pop"]
  0 -> 1 [label="dl: pop"]
  0 -> 6 [label="dr: push 5"]
  1 -> 7 [label="rl: noop"]
  1 -> 7 [label="rr: noop"]
  1 -> 9 [label="dl: divide"]
  1 -> 9 [label="dr: divide"]
  1 -> 6 [label="ll: pop"]
  1 -> 6 [label="lr: pop"]
  2 -> 4 [label="rl: noop"]
  2 -> 4 [label="rr: noop"]
  2 -> 9 [label="dl: noop"]
  2 -> 1 [label="dr: roll"]
  2 -> 1 [label="ll: roll"]
  2 -> 1 [label="lr: roll"]
  4 -> 5 [label="rl: out (char)"]
  4 -> 5 [label="rr: out (char)"]
  4 -> 7 [label="dl: subtract"]
  4 -> 7 [label="dr: noop"]
  4 -> 2 [label="ll: noop"]
  4 -> 2 [label="lr: noop"]
  5 -> 7 [label="dr: not"]
  5 -> 4 [label="ll: subtract"]
  5 -> 4 [label="lr: subtract"]
  6 -> 9 [label="rl: mod"]
  6 -> 9 [label="rr: mod"]
  6 -> 9 [label="dl: mod"]
  6 -> 12 [label="dr: in (char)"]
  6 -> 0 [label="ul: pop"]
  6 -> 0 [label="ur: pop"]
  7 -> 9 [label="ll: noop"]
  7 -> 9 [label="lr: noop"]
  7 -> 4 [label="ul: out (char)"]
  7 -> 5 [label="ur: roll"]
  9 -> 17 [label="dl: push 16"]
  9 -> 17 [label="dr: push 16"]
  9 -> 12 [label="ll: switch"]
  9 -> 12 [label="lr: switch"]
  9 -> 1 [label="ul: duplicate"]
  9 -> 1 [label="ur: duplicate"]
  12 -> 9 [label="rl: pointer"]
  12 -> 9 [label="rr: pointer"]
  12 -> 23 [label="dl: noop"]
  12 -> 22 [label="dr: noop -> ll"]
  12 -> 6 [label="ul: add"]
  12 -> 6 [label="ur: add"]
  15 -> 18 [label="dl: duplicate"]
  15 -> 18 [label="dr: duplicate"]
  15 -> 9 [label="ll: roll"]
  15 -> 9 [label="lr: roll"]
  15 -> 9 [label="ul: roll"]
  15 -> 9 [label="ur: roll"]
  17 -> 18 [label="rl: push 1"]
  17 -> 18 [label="rr: push 1"]
  17 -> 23 [label="dl: noop -> lr"]
  17 -> 23 [label="dr: noop -> ll"]
  17 -> 12 [label="ll: noop -> ur"]
  17 -> 12 [label="lr: noop -> ul"]
  17 -> 9 [label="ul: pop"]
  17 -> 9 [label="ur: pop"]
  18 -> 25 [label="dl: divide"]
  18 -> 25 [label="dr: divide"]
  18 -> 17 [label="ll: pop"]
  18 -> 17 [label="lr: pop"]
  18 -> 15 [label="ul: divide"]
  22 -> 23 [label="rl: noop"]
  22 -> 23 [label="rr: noop"]
  22 -> 12 [label="ll: noop -> ur"]
  22 -> 12 [label="lr: noop -> ul"]
  22 -> 12 [label="ul: noop"]
  22 -> 12 [label="ur: noop"]
  23 -> 25 [label="rl: noop"]
  23 -> 25 [label="rr: noop"]
  23 -> 22 [label="ll: noop"]
  23 -> 22 [label="lr: noop"]
  23 -> 12 [label="ul: noop"]
  23 -> 12 [label="ur: noop"]
  25 -> 25 [label="rl: noop -> ll"]
  25 -> 25 [label="rr: noop -> lr"]
  25 -> 23 [label="ll: noop"]
  25 -> 23 [label="lr: noop"]
  25 -> 18 [label="ul: duplicate"]
  25 -> 18 [label="ur: duplicate"]
}|]
