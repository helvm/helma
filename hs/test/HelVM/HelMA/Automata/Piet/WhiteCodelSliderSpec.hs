{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.WhiteCodelSliderSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.TestUtils
import           HelVM.HelMA.Automata.Piet.WhiteCodelSlider

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Codel
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Cursor         hiding ( initialCursor )
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness
import           HelVM.HelMA.Automata.Piet.Types.Matrix
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified Data.Vector                                    as V

import           Test.Hspec
import           Text.InterpolatedString.Perl6

data TestCase
  = TestCase
      { caseName       :: String
      , testImage      :: Matrix Codel
      , initialCursor  :: Cursor
      , expectedResult :: Maybe NextBlock
      }

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = describe "slideOnWhiteBlock" $ forM_ testCases runTest where
  runTest tc =
    context ("when given " ++ caseName tc) $
      it "slide and pure the next codel" $
        slideOnWhiteBlock (testImage tc) (initialCursor tc) `shouldBe` expectedResult tc

  testCases =
    [ TestCase "singleCodelImage (0, 0) rl" singleCodelImage (Cursor (0, 0) rl) Nothing
    , TestCase "oneLoopImage (1, 1) rl" oneLoopImage (Cursor (1, 1) rl) (Just $ NextBlock NoOperation (BlockEdge 2 ur))
    , TestCase "oneLoopImage (1, 1) rr" oneLoopImage (Cursor (1, 1) rr) (Just $ NextBlock NoOperation (BlockEdge 2 ul))
    , TestCase "gammaImage (1, 1) rl" gammaImage (Cursor (1, 1) rl) (Just $ NextBlock NoOperation (BlockEdge 1 rl))
    , TestCase "gammaImage (1, 4) rl" gammaImage (Cursor (1, 4) rl) (Just $ NextBlock NoOperation (BlockEdge 0 ll))
    , TestCase "crossShapedImage (2, 1) rl" crossShapedImage (Cursor (2, 1) rl) (Just $ NextBlock NoOperation (BlockEdge 5 dr))
    , TestCase "crossShapedImage (2, 1) rr" crossShapedImage (Cursor (2, 1) rr) (Just $ NextBlock NoOperation (BlockEdge 5 dl))
    , TestCase "crossShapedImage (2, 1) dl" crossShapedImage (Cursor (2, 1) dl) (Just $ NextBlock NoOperation (BlockEdge 5 dl))
    , TestCase "crossShapedImage (2, 1) dr" crossShapedImage (Cursor (2, 1) dr) (Just $ NextBlock NoOperation (BlockEdge 5 dr))
    , TestCase "crossShapedImage (2, 1) ll" crossShapedImage (Cursor (2, 1) ll) (Just $ NextBlock NoOperation (BlockEdge 5 dr))
    , TestCase "crossShapedImage (2, 1) lr" crossShapedImage (Cursor (2, 1) lr) (Just $ NextBlock NoOperation (BlockEdge 5 dl))
    , TestCase "crossShapedImage (2, 1) ul" crossShapedImage (Cursor (2, 1) ul) (Just $ NextBlock NoOperation (BlockEdge 5 dl))
    , TestCase "crossShapedImage (2, 1) ur" crossShapedImage (Cursor (2, 1) ur) (Just $ NextBlock NoOperation (BlockEdge 5 dr))
    , TestCase "crossShapedImage (1, 2) rl" crossShapedImage (Cursor (1, 2) rl) (Just $ NextBlock NoOperation (BlockEdge 5 rl))
    , TestCase "crossShapedImage (1, 2) rr" crossShapedImage (Cursor (1, 2) rr) (Just $ NextBlock NoOperation (BlockEdge 5 rr))
    , TestCase "crossShapedImage (1, 2) dl" crossShapedImage (Cursor (1, 2) dl) (Just $ NextBlock NoOperation (BlockEdge 5 rr))
    , TestCase "crossShapedImage (1, 2) dr" crossShapedImage (Cursor (1, 2) dr) (Just $ NextBlock NoOperation (BlockEdge 5 rl))
    , TestCase "crossShapedImage (1, 2) ll" crossShapedImage (Cursor (1, 2) ll) (Just $ NextBlock NoOperation (BlockEdge 5 rl))
    , TestCase "crossShapedImage (1, 2) lr" crossShapedImage (Cursor (1, 2) lr) (Just $ NextBlock NoOperation (BlockEdge 5 rr))
    , TestCase "crossShapedImage (1, 2) ul" crossShapedImage (Cursor (1, 2) ul) (Just $ NextBlock NoOperation (BlockEdge 5 rr))
    , TestCase "crossShapedImage (1, 2) ur" crossShapedImage (Cursor (1, 2) ur) (Just $ NextBlock NoOperation (BlockEdge 5 rl))
    , TestCase "crossShapedImage (3, 2) rl" crossShapedImage (Cursor (3, 2) rl) (Just $ NextBlock NoOperation (BlockEdge 5 ll))
    , TestCase "crossShapedImage (3, 2) rr" crossShapedImage (Cursor (3, 2) rr) (Just $ NextBlock NoOperation (BlockEdge 5 lr))
    , TestCase "crossShapedImage (3, 2) dl" crossShapedImage (Cursor (3, 2) dl) (Just $ NextBlock NoOperation (BlockEdge 5 lr))
    , TestCase "crossShapedImage (3, 2) dr" crossShapedImage (Cursor (3, 2) dr) (Just $ NextBlock NoOperation (BlockEdge 5 ll))
    , TestCase "crossShapedImage (3, 2) ll" crossShapedImage (Cursor (3, 2) ll) (Just $ NextBlock NoOperation (BlockEdge 5 ll))
    , TestCase "crossShapedImage (3, 2) lr" crossShapedImage (Cursor (3, 2) lr) (Just $ NextBlock NoOperation (BlockEdge 5 lr))
    , TestCase "crossShapedImage (3, 2) ul" crossShapedImage (Cursor (3, 3) ul) (Just $ NextBlock NoOperation (BlockEdge 5 lr))
    , TestCase "crossShapedImage (3, 2) ur" crossShapedImage (Cursor (3, 2) ur) (Just $ NextBlock NoOperation (BlockEdge 5 ll))
    , TestCase "crossShapedImage (2, 3) rl" crossShapedImage (Cursor (2, 3) rl) (Just $ NextBlock NoOperation (BlockEdge 5 ur))
    , TestCase "crossShapedImage (2, 3) rr" crossShapedImage (Cursor (2, 3) rr) (Just $ NextBlock NoOperation (BlockEdge 5 ul))
    , TestCase "crossShapedImage (2, 3) dl" crossShapedImage (Cursor (2, 3) dl) (Just $ NextBlock NoOperation (BlockEdge 5 ul))
    , TestCase "crossShapedImage (2, 3) dr" crossShapedImage (Cursor (2, 3) dr) (Just $ NextBlock NoOperation (BlockEdge 5 ur))
    , TestCase "crossShapedImage (2, 3) ll" crossShapedImage (Cursor (2, 3) ll) (Just $ NextBlock NoOperation (BlockEdge 5 ur))
    , TestCase "crossShapedImage (2, 3) lr" crossShapedImage (Cursor (2, 3) lr) (Just $ NextBlock NoOperation (BlockEdge 5 ul))
    , TestCase "crossShapedImage (2, 3) ul" crossShapedImage (Cursor (2, 3) ul) (Just $ NextBlock NoOperation (BlockEdge 5 ul))
    , TestCase "crossShapedImage (2, 3) ur" crossShapedImage (Cursor (2, 3) ur) (Just $ NextBlock NoOperation (BlockEdge 5 ur))
    , TestCase "spiralImage (1, 1) rl" spiralImage (Cursor (1, 1) rl) (Just $ NextBlock NoOperation (BlockEdge 4 rl))
    , TestCase "stuckImage1 (1, 1) rl" stuckImage1 (Cursor (1, 1) rl) Nothing
    , TestCase "stuckImage2 (1, 1) rl" stuckImage2 (Cursor (1, 1) rl) Nothing
    , TestCase "stuckImage3 (1, 1) rl" stuckImage3 (Cursor (1, 1) rl) Nothing
    ]

singleCodelImage ∷ Matrix Codel
singleCodelImage = V.singleton $ V.singleton $ Codel White 0

oneLoopImage ∷ Matrix Codel
oneLoopImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y2         1
|] ∷ String))) where
  f '0' = Codel Black 0
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 1
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 2
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 3
  f '1' = Codel Black 4
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 5
  f ' ' = Codel White 6
  f '2' = Codel Black 7
  f _   = error "Unreachable"

gammaImage ∷ Matrix Codel
gammaImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
r          g
r       0  b
r          c
r          m
r     1    y
|] ∷ String))) where
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 0
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 1
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 2
  f 'c' = Codel (Chromatic $ ChromaticColor Cyan Normal) 3
  f 'm' = Codel (Chromatic $ ChromaticColor Magenta Normal) 4
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 5
  f ' ' = Codel White 6
  f '0' = Codel Black 7
  f '1' = Codel Black 8
  f _   = error "Unreachable"

crossShapedImage ∷ Matrix Codel
crossShapedImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
*****
**0**
*1r2*
**3**
*****
|] ∷ String))) where
  f '*' = Codel Black 0
  f '0' = Codel White 1
  f '1' = Codel White 2
  f '2' = Codel White 3
  f '3' = Codel White 4
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 5
  f _   = error "Unreachable"

spiralImage ∷ Matrix Codel
spiralImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
rrrrrrrrrrrg
y         0g
y 3        g
y       4  g
y   7      g
y     c    g
y  6       g
y      5   g
y2         g
y        1 g
ybbbbbbbbbbb
|] ∷ String))) where
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 0
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 1
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 2
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 3
  f 'c' = Codel (Chromatic $ ChromaticColor Cyan Normal) 4
  f ' ' = Codel White 5
  f '0' = Codel Black 6
  f '1' = Codel Black 7
  f '2' = Codel Black 8
  f '3' = Codel Black 9
  f '4' = Codel Black 10
  f '5' = Codel Black 11
  f '6' = Codel Black 12
  f '7' = Codel Black 13
  f _   = error "Unreachable"

stuckImage1 ∷ Matrix Codel
stuckImage1 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y       2  1
|] ∷ String))) where
  f '0' = Codel Black 0
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 1
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 2
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 3
  f '1' = Codel Black 4
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 5
  f ' ' = Codel White 6
  f '2' = Codel Black 7
  f _   = error "Unreachable"

stuckImage2 ∷ Matrix Codel
stuckImage2 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y        2 1
|] ∷ String))) where
  f '0' = Codel Black 0
  f 'r' = Codel (Chromatic $ ChromaticColor Red Normal) 1
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 2
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 3
  f '1' = Codel Black 4
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 5
  f ' ' = Codel White 6
  f '2' = Codel Black 7
  f _   = error "Unreachable"

stuckImage3 ∷ Matrix Codel
stuckImage3 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
00gb11111111
y          1
y          1
y          1
y          1
2          1
|] ∷ String))) where
  f '0' = Codel Black 0
  f 'g' = Codel (Chromatic $ ChromaticColor Green Normal) 1
  f 'b' = Codel (Chromatic $ ChromaticColor Blue Normal) 2
  f '1' = Codel Black 3
  f 'y' = Codel (Chromatic $ ChromaticColor Yellow Normal) 4
  f ' ' = Codel White 5
  f '2' = Codel Black 6
  f _   = error "Unreachable"
