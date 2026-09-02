{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.SyntaxVisualizerSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.SyntaxVisualizer
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import           HelVM.HelMA.Automata.Piet.Types.Command

import qualified Data.IntMap                                 as IM
import qualified Data.Map                                    as M

import           Test.Hspec
import           Text.InterpolatedString.Perl6

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "syntaxToDOT" $
    forM_
      [ ("emptyGraph", Nothing, "digraph {}")
      , ("smallestGraph", smallestGraph, smallestDOT)
      , ("stuckGraph", stuckGraph, stuckDOT)
      , ("complexGraph", complexGraph, complexDOT)
      ] $ \(name, graph, dot) ->
        context ("when given " ++ name) $
          it "convert a syntax graph to a DOT script" $ syntaxToDOT graph `shouldBe` dot

smallestGraph ∷ Maybe SyntaxGraph
smallestGraph = Just $ SyntaxGraph (BlockEdge 999 dr) $ one (999 , Block M.empty)

smallestDOT ∷ LText
smallestDOT = [q|digraph {
  rankdir=LR
  start [label="" shape=point color=white]
  node [label="" shape=circle color=black]
  start -> 999 [label="dr"]
  exit999 [label="" shape=point color=white]
  999 -> exit999 [label=""]
}|]

stuckGraph ∷ Maybe SyntaxGraph
stuckGraph = Just $ SyntaxGraph (BlockEdge 0 rl) $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, Just $ NextBlock (Push 1) (BlockEdge 1 rl))
                         , (rr, Just $ NextBlock (Push 1) (BlockEdge 1 rr))
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 0 ul))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 0 ur))
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, Nothing)
                         , (rr, Nothing)
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 0 ul))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 0 ur))
                         , (ll, Just $ NextBlock Pop (BlockEdge 0 ll))
                         , (lr, Just $ NextBlock Pop (BlockEdge 0 lr))
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

complexGraph ∷ Maybe SyntaxGraph
complexGraph = Just $ SyntaxGraph (BlockEdge 0 rl) $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, Just $ NextBlock Pop (BlockEdge 1 rl))
                         , (rr, Just $ NextBlock Pop (BlockEdge 1 rr))
                         , (dl, Just $ NextBlock Pop (BlockEdge 1 dl))
                         , (dr, Just $ NextBlock (Push 5) (BlockEdge 6 dr))
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 7 rl))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 7 rr))
                         , (dl, Just $ NextBlock Divide (BlockEdge 9 dl))
                         , (dr, Just $ NextBlock Divide (BlockEdge 9 dr))
                         , (ll, Just $ NextBlock Pop (BlockEdge 6 ll))
                         , (lr, Just $ NextBlock Pop (BlockEdge 6 lr))
                         ]
    )
  , ( 2
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 4 rl))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 4 rr))
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 9 dl))
                         , (dr, Just $ NextBlock Roll (BlockEdge 1 dr))
                         , (ll, Just $ NextBlock Roll (BlockEdge 1 ll))
                         , (lr, Just $ NextBlock Roll (BlockEdge 1 lr))
                         ]
    )
  , ( 4
    , Block $ M.fromList [ (rl, Just $ NextBlock OutChar (BlockEdge 5 rl))
                         , (rr, Just $ NextBlock OutChar (BlockEdge 5 rr))
                         , (dl, Just $ NextBlock Subtract (BlockEdge 7 dl))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 7 dr))
                         , (ll, Just $ NextBlock NoOperation (BlockEdge 2 ll))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 2 lr))
                         ]
    )
  , ( 5
    , Block $ M.fromList [ (dr, Just $ NextBlock Not (BlockEdge 7 dr))
                         , (ll, Just $ NextBlock Subtract (BlockEdge 4 ll))
                         , (lr, Just $ NextBlock Subtract (BlockEdge 4 lr))
                         ]
    )
  , ( 6
    , Block $ M.fromList [ (rl, Just $ NextBlock Mod (BlockEdge 9 rl))
                         , (rr, Just $ NextBlock Mod (BlockEdge 9 rr))
                         , (dl, Just $ NextBlock Mod (BlockEdge 9 dl))
                         , (dr, Just $ NextBlock InChar (BlockEdge 12 dr))
                         , (ul, Just $ NextBlock Pop (BlockEdge 0 ul))
                         , (ur, Just $ NextBlock Pop (BlockEdge 0 ur))
                         ]
    )
  , ( 7
    , Block $ M.fromList [ (ll, Just $ NextBlock NoOperation (BlockEdge 9 ll))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 9 lr))
                         , (ul, Just $ NextBlock OutChar (BlockEdge 4 ul))
                         , (ur, Just $ NextBlock Roll (BlockEdge 5 ur))
                         ]
    )
  , ( 9
    , Block $ M.fromList [ (dl, Just $ NextBlock (Push 16) (BlockEdge 17 dl))
                         , (dr, Just $ NextBlock (Push 16) (BlockEdge 17 dr))
                         , (ll, Just $ NextBlock Switch (BlockEdge 12 ll))
                         , (lr, Just $ NextBlock Switch (BlockEdge 12 lr))
                         , (ul, Just $ NextBlock Duplicate (BlockEdge 1 ul))
                         , (ur, Just $ NextBlock Duplicate (BlockEdge 1 ur))
                         ]
    )
  , ( 12
    , Block $ M.fromList [ (rl, Just $ NextBlock Pointer (BlockEdge 9 rl))
                         , (rr, Just $ NextBlock Pointer (BlockEdge 9 rr))
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 23 dl))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 22 ll))
                         , (ul, Just $ NextBlock Add (BlockEdge 6 ul))
                         , (ur, Just $ NextBlock Add (BlockEdge 6 ur))
                         ]
    )
  , ( 15
    , Block $ M.fromList [ (dl, Just $ NextBlock Duplicate (BlockEdge 18 dl))
                         , (dr, Just $ NextBlock Duplicate (BlockEdge 18 dr))
                         , (ll, Just $ NextBlock Roll (BlockEdge 9 ll))
                         , (lr, Just $ NextBlock Roll (BlockEdge 9 lr))
                         , (ul, Just $ NextBlock Roll (BlockEdge 9 ul))
                         , (ur, Just $ NextBlock Roll (BlockEdge 9 ur))
                         ]
    )
  , ( 17
    , Block $ M.fromList [ (rl, Just $ NextBlock (Push 1) (BlockEdge 18 rl))
                         , (rr, Just $ NextBlock (Push 1) (BlockEdge 18 rr))
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 23 lr))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 23 ll))
                         , (ll, Just $ NextBlock NoOperation (BlockEdge 12 ur))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 12 ul))
                         , (ul, Just $ NextBlock Pop (BlockEdge 9 ul))
                         , (ur, Just $ NextBlock Pop (BlockEdge 9 ur))
                         ]
    )
  , ( 18
    , Block $ M.fromList [ (dl, Just $ NextBlock Divide (BlockEdge 25 dl))
                         , (dr, Just $ NextBlock Divide (BlockEdge 25 dr))
                         , (ll, Just $ NextBlock Pop (BlockEdge 17 ll))
                         , (lr, Just $ NextBlock Pop (BlockEdge 17 lr))
                         , (ul, Just $ NextBlock Divide (BlockEdge 15 ul))
                         ]
    )
  , ( 22
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 23 rl))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 23 rr))
                         , (ll, Just $ NextBlock NoOperation (BlockEdge 12 ur))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 12 ul))
                         , (ul, Just $ NextBlock NoOperation (BlockEdge 12 ul))
                         , (ur, Just $ NextBlock NoOperation (BlockEdge 12 ur))
                         ]
    )
  , ( 23
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 25 rl))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 25 rr))
                         , (ll, Just $ NextBlock NoOperation (BlockEdge 22 ll))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 22 lr))
                         , (ul, Just $ NextBlock NoOperation (BlockEdge 12 ul))
                         , (ur, Just $ NextBlock NoOperation (BlockEdge 12 ur))
                         ]
    )
  , ( 25
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 25 ll))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 25 lr))
                         , (ll, Just $ NextBlock NoOperation (BlockEdge 23 ll))
                         , (lr, Just $ NextBlock NoOperation (BlockEdge 23 lr))
                         , (ul, Just $ NextBlock Duplicate (BlockEdge 18 ul))
                         , (ur, Just $ NextBlock Duplicate (BlockEdge 18 ur))
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
