{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.LLVM.Piet.SyntaxVisualizerSpec
  ( main
  , spec
  ) where

import qualified Data.IntMap                                          as IM
import qualified Data.Map                                             as M
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Syntax
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
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

smallestGraph ∷ SyntaxGraph
smallestGraph = SyntaxGraph 999 dr $ one (999 , Block M.empty)

smallestDOT ∷ LText
smallestDOT = [q|digraph {
  rankdir=LR
  start [label="" shape=point color=white]
  node [label="" shape=circle color=black]
  start -> 999 [label="dr"]
  exit999 [label="" shape=point color=white]
  999 -> exit999 [label=""]
}|]

stuckGraph ∷ SyntaxGraph
stuckGraph = SyntaxGraph 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, NextBlock (Push 1) rl 1)
                         , (rr, NextBlock (Push 1) rr 1)
                         , (dl, NextBlock NoOperation ul 0)
                         , (dr, NextBlock NoOperation ur 0)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, ExitProgram)
                         , (rr, ExitProgram)
                         , (dl, NextBlock NoOperation ul 0)
                         , (dr, NextBlock NoOperation ur 0)
                         , (ll, NextBlock Pop ll 0)
                         , (lr, NextBlock Pop lr 0)
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

complexGraph ∷ SyntaxGraph
complexGraph = SyntaxGraph 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, NextBlock Pop rl 1)
                         , (rr, NextBlock Pop rr 1)
                         , (dl, NextBlock Pop dl 1)
                         , (dr, NextBlock (Push 5) dr 6)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, NextBlock NoOperation rl 7)
                         , (rr, NextBlock NoOperation rr 7)
                         , (dl, NextBlock Divide dl 9)
                         , (dr, NextBlock Divide dr 9)
                         , (ll, NextBlock Pop ll 6)
                         , (lr, NextBlock Pop lr 6)
                         ]
    )
  , ( 2
    , Block $ M.fromList [ (rl, NextBlock NoOperation rl 4)
                         , (rr, NextBlock NoOperation rr 4)
                         , (dl, NextBlock NoOperation dl 9)
                         , (dr, NextBlock Roll dr 1)
                         , (ll, NextBlock Roll ll 1)
                         , (lr, NextBlock Roll lr 1)
                         ]
    )
  , ( 4
    , Block $ M.fromList [ (rl, NextBlock OutChar rl 5)
                         , (rr, NextBlock OutChar rr 5)
                         , (dl, NextBlock Subtract dl 7)
                         , (dr, NextBlock NoOperation dr 7)
                         , (ll, NextBlock NoOperation ll 2)
                         , (lr, NextBlock NoOperation lr 2)
                         ]
    )
  , ( 5
    , Block $ M.fromList [ (dr, NextBlock Not dr 7)
                         , (ll, NextBlock Subtract ll 4)
                         , (lr, NextBlock Subtract lr 4)
                         ]
    )
  , ( 6
    , Block $ M.fromList [ (rl, NextBlock Mod rl 9)
                         , (rr, NextBlock Mod rr 9)
                         , (dl, NextBlock Mod dl 9)
                         , (dr, NextBlock InChar dr 12)
                         , (ul, NextBlock Pop ul 0)
                         , (ur, NextBlock Pop ur 0)
                         ]
    )
  , ( 7
    , Block $ M.fromList [ (ll, NextBlock NoOperation ll 9)
                         , (lr, NextBlock NoOperation lr 9)
                         , (ul, NextBlock OutChar ul 4)
                         , (ur, NextBlock Roll ur 5)
                         ]
    )
  , ( 9
    , Block $ M.fromList [ (dl, NextBlock (Push 16) dl 17)
                         , (dr, NextBlock (Push 16) dr 17)
                         , (ll, NextBlock Switch ll 12)
                         , (lr, NextBlock Switch lr 12)
                         , (ul, NextBlock Duplicate ul 1)
                         , (ur, NextBlock Duplicate ur 1)
                         ]
    )
  , ( 12
    , Block $ M.fromList [ (rl, NextBlock Pointer rl 9)
                         , (rr, NextBlock Pointer rr 9)
                         , (dl, NextBlock NoOperation dl 23)
                         , (dr, NextBlock NoOperation ll 22)
                         , (ul, NextBlock Add ul 6)
                         , (ur, NextBlock Add ur 6)
                         ]
    )
  , ( 15
    , Block $ M.fromList [ (dl, NextBlock Duplicate dl 18)
                         , (dr, NextBlock Duplicate dr 18)
                         , (ll, NextBlock Roll ll 9)
                         , (lr, NextBlock Roll lr 9)
                         , (ul, NextBlock Roll ul 9)
                         , (ur, NextBlock Roll ur 9)
                         ]
    )
  , ( 17
    , Block $ M.fromList [ (rl, NextBlock (Push 1) rl 18)
                         , (rr, NextBlock (Push 1) rr 18)
                         , (dl, NextBlock NoOperation lr 23)
                         , (dr, NextBlock NoOperation ll 23)
                         , (ll, NextBlock NoOperation ur 12)
                         , (lr, NextBlock NoOperation ul 12)
                         , (ul, NextBlock Pop ul 9)
                         , (ur, NextBlock Pop ur 9)
                         ]
    )
  , ( 18
    , Block $ M.fromList [ (dl, NextBlock Divide dl 25)
                         , (dr, NextBlock Divide dr 25)
                         , (ll, NextBlock Pop ll 17)
                         , (lr, NextBlock Pop lr 17)
                         , (ul, NextBlock Divide ul 15)
                         ]
    )
  , ( 22
    , Block $ M.fromList [ (rl, NextBlock NoOperation rl 23)
                         , (rr, NextBlock NoOperation rr 23)
                         , (ll, NextBlock NoOperation ur 12)
                         , (lr, NextBlock NoOperation ul 12)
                         , (ul, NextBlock NoOperation ul 12)
                         , (ur, NextBlock NoOperation ur 12)
                         ]
    )
  , ( 23
    , Block $ M.fromList [ (rl, NextBlock NoOperation rl 25)
                         , (rr, NextBlock NoOperation rr 25)
                         , (ll, NextBlock NoOperation ll 22)
                         , (lr, NextBlock NoOperation lr 22)
                         , (ul, NextBlock NoOperation ul 12)
                         , (ur, NextBlock NoOperation ur 12)
                         ]
    )
  , ( 25
    , Block $ M.fromList [ (rl, NextBlock NoOperation ll 25)
                         , (rr, NextBlock NoOperation lr 25)
                         , (ll, NextBlock NoOperation ll 23)
                         , (lr, NextBlock NoOperation lr 23)
                         , (ul, NextBlock Duplicate ul 18)
                         , (ur, NextBlock Duplicate ur 18)
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
