{-# LANGUAGE QuasiQuotes #-}

module HelVM.HelMA.Automata.Piet.LLVM.Internal.WhiteCodelSliderSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils
import           HelVM.HelMA.Automata.Piet.LLVM.WhiteCodelSlider

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Course          hiding ( initialCourse )
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Data.Vector                                     ( Vector )
import qualified Data.Vector                                     as V

import           Test.Hspec
import           Text.InterpolatedString.Perl6

data TestCase
  = TestCase
      { caseName        :: String
      , testImage       :: Vector (Vector (Color, Int))
      , initialPosition :: Coordinates
      , initialCourse   :: Course
      , expectedResult  :: NextBlockMaybe
      }

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = describe "slideOnWhiteBlock" $ forM_ testCases runTest where
  runTest tc =
    context ("when given " ++ caseName tc) $
      it "slide and pure the next codel" $
        slideOnWhiteBlock (testImage tc) (initialPosition tc) (initialCourse tc) `shouldBe` expectedResult tc

  testCases =
    [ TestCase "singleCodelImage (0, 0) rl" singleCodelImage (0, 0) rl ExitProgram
    , TestCase "oneLoopImage (1, 1) rl" oneLoopImage (1, 1) rl (NextBlockJust $ NextBlock NoOperation ur 2)
    , TestCase "oneLoopImage (1, 1) rr" oneLoopImage (1, 1) rr (NextBlockJust $ NextBlock NoOperation ul 2)
    , TestCase "gammaImage (1, 1) rl" gammaImage (1, 1) rl (NextBlockJust $ NextBlock NoOperation rl 1)
    , TestCase "gammaImage (1, 4) rl" gammaImage (1, 4) rl (NextBlockJust $ NextBlock NoOperation ll 0)
    , TestCase "crossShapedImage (2, 1) rl" crossShapedImage (2, 1) rl (NextBlockJust $ NextBlock NoOperation dr 5)
    , TestCase "crossShapedImage (2, 1) rr" crossShapedImage (2, 1) rr (NextBlockJust $ NextBlock NoOperation dl 5)
    , TestCase "crossShapedImage (2, 1) dl" crossShapedImage (2, 1) dl (NextBlockJust $ NextBlock NoOperation dl 5)
    , TestCase "crossShapedImage (2, 1) dr" crossShapedImage (2, 1) dr (NextBlockJust $ NextBlock NoOperation dr 5)
    , TestCase "crossShapedImage (2, 1) ll" crossShapedImage (2, 1) ll (NextBlockJust $ NextBlock NoOperation dr 5)
    , TestCase "crossShapedImage (2, 1) lr" crossShapedImage (2, 1) lr (NextBlockJust $ NextBlock NoOperation dl 5)
    , TestCase "crossShapedImage (2, 1) ul" crossShapedImage (2, 1) ul (NextBlockJust $ NextBlock NoOperation dl 5)
    , TestCase "crossShapedImage (2, 1) ur" crossShapedImage (2, 1) ur (NextBlockJust $ NextBlock NoOperation dr 5)
    , TestCase "crossShapedImage (1, 2) rl" crossShapedImage (1, 2) rl (NextBlockJust $ NextBlock NoOperation rl 5)
    , TestCase "crossShapedImage (1, 2) rr" crossShapedImage (1, 2) rr (NextBlockJust $ NextBlock NoOperation rr 5)
    , TestCase "crossShapedImage (1, 2) dl" crossShapedImage (1, 2) dl (NextBlockJust $ NextBlock NoOperation rr 5)
    , TestCase "crossShapedImage (1, 2) dr" crossShapedImage (1, 2) dr (NextBlockJust $ NextBlock NoOperation rl 5)
    , TestCase "crossShapedImage (1, 2) ll" crossShapedImage (1, 2) ll (NextBlockJust $ NextBlock NoOperation rl 5)
    , TestCase "crossShapedImage (1, 2) lr" crossShapedImage (1, 2) lr (NextBlockJust $ NextBlock NoOperation rr 5)
    , TestCase "crossShapedImage (1, 2) ul" crossShapedImage (1, 2) ul (NextBlockJust $ NextBlock NoOperation rr 5)
    , TestCase "crossShapedImage (1, 2) ur" crossShapedImage (1, 2) ur (NextBlockJust $ NextBlock NoOperation rl 5)
    , TestCase "crossShapedImage (3, 2) rl" crossShapedImage (3, 2) rl (NextBlockJust $ NextBlock NoOperation ll 5)
    , TestCase "crossShapedImage (3, 2) rr" crossShapedImage (3, 2) rr (NextBlockJust $ NextBlock NoOperation lr 5)
    , TestCase "crossShapedImage (3, 2) dl" crossShapedImage (3, 2) dl (NextBlockJust $ NextBlock NoOperation lr 5)
    , TestCase "crossShapedImage (3, 2) dr" crossShapedImage (3, 2) dr (NextBlockJust $ NextBlock NoOperation ll 5)
    , TestCase "crossShapedImage (3, 2) ll" crossShapedImage (3, 2) ll (NextBlockJust $ NextBlock NoOperation ll 5)
    , TestCase "crossShapedImage (3, 2) lr" crossShapedImage (3, 2) lr (NextBlockJust $ NextBlock NoOperation lr 5)
    , TestCase "crossShapedImage (3, 2) ul" crossShapedImage (3, 3) ul (NextBlockJust $ NextBlock NoOperation lr 5)
    , TestCase "crossShapedImage (3, 2) ur" crossShapedImage (3, 2) ur (NextBlockJust $ NextBlock NoOperation ll 5)
    , TestCase "crossShapedImage (2, 3) rl" crossShapedImage (2, 3) rl (NextBlockJust $ NextBlock NoOperation ur 5)
    , TestCase "crossShapedImage (2, 3) rr" crossShapedImage (2, 3) rr (NextBlockJust $ NextBlock NoOperation ul 5)
    , TestCase "crossShapedImage (2, 3) dl" crossShapedImage (2, 3) dl (NextBlockJust $ NextBlock NoOperation ul 5)
    , TestCase "crossShapedImage (2, 3) dr" crossShapedImage (2, 3) dr (NextBlockJust $ NextBlock NoOperation ur 5)
    , TestCase "crossShapedImage (2, 3) ll" crossShapedImage (2, 3) ll (NextBlockJust $ NextBlock NoOperation ur 5)
    , TestCase "crossShapedImage (2, 3) lr" crossShapedImage (2, 3) lr (NextBlockJust $ NextBlock NoOperation ul 5)
    , TestCase "crossShapedImage (2, 3) ul" crossShapedImage (2, 3) ul (NextBlockJust $ NextBlock NoOperation ul 5)
    , TestCase "crossShapedImage (2, 3) ur" crossShapedImage (2, 3) ur (NextBlockJust $ NextBlock NoOperation ur 5)
    , TestCase "spiralImage (1, 1) rl" spiralImage (1, 1) rl (NextBlockJust $ NextBlock NoOperation rl 4)
    , TestCase "stuckImage1 (1, 1) rl" stuckImage1 (1, 1) rl ExitProgram
    , TestCase "stuckImage2 (1, 1) rl" stuckImage2 (1, 1) rl ExitProgram
    , TestCase "stuckImage3 (1, 1) rl" stuckImage3 (1, 1) rl ExitProgram
    ]

singleCodelImage ∷ Vector (Vector (Color, Int))
singleCodelImage = V.singleton $ V.singleton (White, 0)

oneLoopImage ∷ Vector (Vector (Color, Int))
oneLoopImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y2         1
|] ∷ String))) where
  f '0' = (Black, 0)
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 1)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 2)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 3)
  f '1' = (Black, 4)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 5)
  f ' ' = (White, 6)
  f '2' = (Black, 7)
  f _   = error "Unreachable"

gammaImage ∷ Vector (Vector (Color, Int))
gammaImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
r          g
r       0  b
r          c
r          m
r     1    y
|] ∷ String))) where
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 0)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 1)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 2)
  f 'c' = (Chromatic $ ChromaticColor Cyan Normal, 3)
  f 'm' = (Chromatic $ ChromaticColor Magenta Normal, 4)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 5)
  f ' ' = (White, 6)
  f '0' = (Black, 7)
  f '1' = (Black, 8)
  f _   = error "Unreachable"

crossShapedImage ∷ Vector (Vector (Color, Int))
crossShapedImage = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
*****
**0**
*1r2*
**3**
*****
|] ∷ String))) where
  f '*' = (Black, 0)
  f '0' = (White, 1)
  f '1' = (White, 2)
  f '2' = (White, 3)
  f '3' = (White, 4)
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 5)
  f _   = error "Unreachable"

spiralImage ∷ Vector (Vector (Color, Int))
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
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 0)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 1)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 2)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 3)
  f 'c' = (Chromatic $ ChromaticColor Cyan Normal, 4)
  f ' ' = (White, 5)
  f '0' = (Black, 6)
  f '1' = (Black, 7)
  f '2' = (Black, 8)
  f '3' = (Black, 9)
  f '4' = (Black, 10)
  f '5' = (Black, 11)
  f '6' = (Black, 12)
  f '7' = (Black, 13)
  f _   = error "Unreachable"

stuckImage1 ∷ Vector (Vector (Color, Int))
stuckImage1 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y       2  1
|] ∷ String))) where
  f '0' = (Black, 0)
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 1)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 2)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 3)
  f '1' = (Black, 4)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 5)
  f ' ' = (White, 6)
  f '2' = (Black, 7)
  f _   = error "Unreachable"

stuckImage2 ∷ Vector (Vector (Color, Int))
stuckImage2 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
0rgb11111111
y          1
y          1
y          1
y          1
y        2 1
|] ∷ String))) where
  f '0' = (Black, 0)
  f 'r' = (Chromatic $ ChromaticColor Red Normal, 1)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 2)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 3)
  f '1' = (Black, 4)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 5)
  f ' ' = (White, 6)
  f '2' = (Black, 7)
  f _   = error "Unreachable"

stuckImage3 ∷ Vector (Vector (Color, Int))
stuckImage3 = V.map (V.map f) $ toVector2D $ toString <$> drop 1 (lines (toText ([q|
00gb11111111
y          1
y          1
y          1
y          1
2          1
|] ∷ String))) where
  f '0' = (Black, 0)
  f 'g' = (Chromatic $ ChromaticColor Green Normal, 1)
  f 'b' = (Chromatic $ ChromaticColor Blue Normal, 2)
  f '1' = (Black, 3)
  f 'y' = (Chromatic $ ChromaticColor Yellow Normal, 4)
  f ' ' = (White, 5)
  f '2' = (Black, 6)
  f _   = error "Unreachable"
