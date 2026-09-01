module HelVM.HelMA.Automata.Piet.LLVM.ParserSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.LLVM.Syntax
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import qualified Data.IntMap                                     as IM
import qualified Data.Map                                        as M
import           Data.Vector                                     ( Vector )
import qualified Data.Vector.Generic                             as V

import           Test.Hspec

data ImageTestCase
  = ImageTestCase
      { caseName      :: String
      , testImage     :: Vector (Vector (Codel, Int))
      , blockTable    :: IntMap [Coordinates]
      , expectedGraph :: SyntaxGraph
      }

data ErrorTestCase
  = ErrorTestCase
      { errCaseName   :: String
      , errTestImage  :: Vector (Vector (Codel, Int))
      , errBlockTable :: IntMap [Coordinates]
      , expectedErr   :: ParserError
      }

data TwoPixelTestCase
  = TwoPixelTestCase
      { color1    :: Codel
      , color2    :: Codel
      , command12 :: Command
      , command21 :: Command
      }

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "parse" $ do
    it "returns a syntax graph when given an image" $ parse rawComplexImage `shouldBe` Right expectedComplexGraph

  describe "parseFilledImage" $ do
    forM_
      [ ImageTestCase "smallImage" smallImage smallBlockTable expectedSmallGraph
      , ImageTestCase "whiteImage" whiteImage whiteBlockTable EmptySyntaxGraph
      , ImageTestCase "distantInitialImage" distantInitialImage distantInitialBlockTable expectedDistantInitialGraph
      , ImageTestCase "stuckImage" stuckImage stuckBlockTable expectedStuckGraph
      , ImageTestCase "complexImage" complexImage complexBlockTable expectedComplexGraph
      ] $ \tc ->
        context ("when given " ++ caseName tc) $ do
          it "returns a syntax graph" $ parseFilledImage (testImage tc, blockTable tc) `shouldBe` Right (expectedGraph tc)

    forM_
      [ ErrorTestCase "emptyImage" V.empty IM.empty EmptyBlockTableError
      , ErrorTestCase "blackImage" blackImage blackBlockTable IllegalInitialColorError
      ] $ \tc ->
        context ("when given " ++ errCaseName tc) $ do
          it "returns an error" $ parseFilledImage (errTestImage tc, errBlockTable tc) `shouldBe` Left (expectedErr tc)

    context "when given an image which only consists of two pixels" $ do
      forM_
        [ TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Red Normal) (Push 1) Pop
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Red Dark) Pop (Push 1)
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Yellow Light) Add InChar
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Yellow Normal) Subtract OutChar
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Yellow Dark) Multiply OutNumber
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Green Light) Divide Duplicate
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Green Normal) Mod InNumber
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Green Dark) Not Roll
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Cyan Light) Greater Greater
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Cyan Normal) Pointer Switch
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Cyan Dark) Switch Pointer
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Blue Light) Duplicate Divide
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Blue Normal) Roll Not
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Blue Dark) InNumber Mod
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Magenta Light) InChar Add
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Magenta Normal) OutNumber Multiply
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Red Light) (AchromaticCodel $ ChromaticColor Magenta Dark) OutChar Subtract
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Cyan Light) (Push 1) Pop
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Cyan Normal) Pop (Push 1)
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Blue Dark) Add InChar
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Blue Light) Subtract OutChar
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Blue Normal) Multiply OutNumber
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Magenta Dark) Divide Duplicate
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Magenta Light) Mod InNumber
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Magenta Normal) Not Roll
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Red Dark) Greater Greater
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Red Light) Pointer Switch
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Red Normal) Switch Pointer
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Yellow Dark) Duplicate Divide
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Yellow Light) Roll Not
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Yellow Normal) InNumber Mod
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Green Dark) InChar Add
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Green Light) OutNumber Multiply
        , TwoPixelTestCase (AchromaticCodel $ ChromaticColor Cyan Dark) (AchromaticCodel $ ChromaticColor Green Normal) OutChar Subtract
        ] $ \tc -> do
          let image = toVector2D [[(color1 tc, 0), (color2 tc, 1)]]
          let bTable = IM.fromList [(0, [(0, 0)]), (1, [(1, 0)])]
          let expectedG = SyntaxGraph 0 rl $
                                IM.fromList [ ( 0
                                              , Block $ M.fromList [ (rl, NextBlock (command12 tc) rl 1)
                                                                   , (rr, NextBlock (command12 tc) rr 1)
                                                                   ]
                                              )
                                            , ( 1
                                              , Block $ M.fromList [ (ll, NextBlock (command21 tc) ll 0)
                                                                   , (lr, NextBlock (command21 tc) lr 0)
                                                                   ]
                                              )
                                            ]
          it ("returns " ++ show (command12 tc, command21 tc) ++ " when given " ++ show (color1 tc, color2 tc)) $ parseFilledImage (image, bTable) `shouldBe` Right expectedG


smallImage ∷ Vector (Vector (Codel, Int))
smallImage = toVector2D [[(AchromaticCodel $ ChromaticColor Red Normal, 0)]]

smallBlockTable ∷ IntMap [Coordinates]
smallBlockTable = IM.fromList [(0, [(0, 0)])]

expectedSmallGraph ∷ SyntaxGraph
expectedSmallGraph = SyntaxGraph 0 rl $ IM.fromList [(0, Block M.empty)]

whiteImage ∷ Vector (Vector (Codel, Int))
whiteImage = toVector2D [[(WhiteCodel, 0)]]

whiteBlockTable ∷ IntMap [Coordinates]
whiteBlockTable = IM.fromList [(0, [(0, 0)])]

blackImage ∷ Vector (Vector (Codel, Int))
blackImage = toVector2D [[(BlackCodel, 0)]]

blackBlockTable ∷ IntMap [Coordinates]
blackBlockTable = IM.fromList [(0, [(0, 0)])]

distantInitialImage ∷ Vector (Vector (Codel, Int))
distantInitialImage = toVector2D
  [ [ (WhiteCodel, 0)
    , (WhiteCodel, 0)
    , (WhiteCodel, 0)
    ]
  , [ (AchromaticCodel $ ChromaticColor Red Normal, 1)
    , (WhiteCodel, 0)
    , (WhiteCodel, 0)
    ]
  , [ (WhiteCodel, 0)
    , (WhiteCodel, 0)
    , (WhiteCodel, 0)
    ]
  ]

distantInitialBlockTable ∷ IntMap [Coordinates]
distantInitialBlockTable = IM.fromList
  [ (0, [(0, 0), (1, 0), (2, 0), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)])
  , (1, [(0, 1)])
  ]

expectedDistantInitialGraph ∷ SyntaxGraph
expectedDistantInitialGraph = SyntaxGraph 1 ur $ IM.fromList
  [ ( 1
    , Block $ M.fromList [ (rl, NextBlock NoOperation ur 1)
                         , (rr, NextBlock NoOperation ul 1)
                         , (dl, NextBlock NoOperation ul 1)
                         , (dr, NextBlock NoOperation ur 1)
                         , (ul, NextBlock NoOperation ul 1)
                         , (ur, NextBlock NoOperation ur 1)
                         ]
    )
  ]

stuckImage ∷ Vector (Vector (Codel, Int))
stuckImage = toVector2D
  [ [ (AchromaticCodel $ ChromaticColor Red Light, 0)
    , (AchromaticCodel $ ChromaticColor Red Normal, 1)
    , (WhiteCodel, 2)
    ]
  , [ (WhiteCodel, 2)
    , (WhiteCodel, 2)
    , (WhiteCodel, 2)
    ]
  , [ (WhiteCodel, 2)
    , (BlackCodel, 3)
    , (WhiteCodel, 2)
    ]
  ]

stuckBlockTable ∷ IntMap [Coordinates]
stuckBlockTable = IM.fromList
  [ (0, [(0, 0)])
  , (1, [(1, 0)])
  , (2, [(2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (2, 2)])
  , (3, [(1, 2)])
  ]

expectedStuckGraph ∷ SyntaxGraph
expectedStuckGraph = SyntaxGraph 0 rl $ IM.fromList
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

rawComplexImage ∷ Vector (Vector Codel)
rawComplexImage = toVector2D
  [ [ AchromaticCodel $ ChromaticColor Blue Dark
    , AchromaticCodel $ ChromaticColor Blue Dark
    , AchromaticCodel $ ChromaticColor Blue Dark
    , AchromaticCodel $ ChromaticColor Blue Dark
    , AchromaticCodel $ ChromaticColor Blue Dark
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Magenta Dark
    , AchromaticCodel $ ChromaticColor Magenta Dark
    , AchromaticCodel $ ChromaticColor Magenta Dark
    ]
  , [ AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , BlackCodel
    ]
  , [ AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Blue Light
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Blue Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , AchromaticCodel $ ChromaticColor Yellow Normal
    , BlackCodel
    , BlackCodel
    , AchromaticCodel $ ChromaticColor Magenta Light
    ]
  , [ AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , AchromaticCodel $ ChromaticColor Magenta Light
    , AchromaticCodel $ ChromaticColor Magenta Light
    ]
  , [ WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Red Normal
    , BlackCodel
    , AchromaticCodel $ ChromaticColor Magenta Light
    , AchromaticCodel $ ChromaticColor Magenta Light
    , BlackCodel
    ]
  , [ WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Cyan Light
    , AchromaticCodel $ ChromaticColor Red Normal
    , AchromaticCodel $ ChromaticColor Green Light
    , BlackCodel
    , BlackCodel
    , AchromaticCodel $ ChromaticColor Magenta Light
    , AchromaticCodel $ ChromaticColor Magenta Light
    , AchromaticCodel $ ChromaticColor Magenta Light
    , BlackCodel
    ]
  , [ WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Red Dark
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , AchromaticCodel $ ChromaticColor Red Light
    , BlackCodel
    , AchromaticCodel $ ChromaticColor Green Dark
    , AchromaticCodel $ ChromaticColor Green Dark
    , AchromaticCodel $ ChromaticColor Red Light
    ]
  , [ WhiteCodel
    , AchromaticCodel $ ChromaticColor Yellow Light
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Cyan Dark
    , AchromaticCodel $ ChromaticColor Cyan Dark
    , WhiteCodel
    , AchromaticCodel $ ChromaticColor Green Light
    , AchromaticCodel $ ChromaticColor Green Light
    , AchromaticCodel $ ChromaticColor Green Light
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , BlackCodel
    ]
  ]

complexImage ∷ Vector (Vector (Codel, Int))
complexImage = toVector2D
  [ [ (AchromaticCodel $ ChromaticColor Blue Dark, 0)
    , (AchromaticCodel $ ChromaticColor Blue Dark, 0)
    , (AchromaticCodel $ ChromaticColor Blue Dark, 0)
    , (AchromaticCodel $ ChromaticColor Blue Dark, 0)
    , (AchromaticCodel $ ChromaticColor Blue Dark, 0)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Red Light, 2)
    , (AchromaticCodel $ ChromaticColor Red Light, 2)
    , (AchromaticCodel $ ChromaticColor Red Light, 2)
    , (WhiteCodel, 3)
    , (AchromaticCodel $ ChromaticColor Red Light, 4)
    , (AchromaticCodel $ ChromaticColor Red Light, 4)
    , (AchromaticCodel $ ChromaticColor Red Light, 4)
    , (AchromaticCodel $ ChromaticColor Magenta Dark, 5)
    , (AchromaticCodel $ ChromaticColor Magenta Dark, 5)
    , (AchromaticCodel $ ChromaticColor Magenta Dark, 5)
    ]
  , [ (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (WhiteCodel, 3)
    , (WhiteCodel, 3)
    , (WhiteCodel, 3)
    , (WhiteCodel, 3)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (BlackCodel, 8)
    ]
  , [ (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Blue Light, 6)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Blue Normal, 1)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (WhiteCodel, 3)
    , (WhiteCodel, 3)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (AchromaticCodel $ ChromaticColor Yellow Normal, 7)
    , (BlackCodel, 10)
    , (BlackCodel, 10)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    ]
  , [ (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (BlackCodel, 10)
    , (BlackCodel, 10)
    , (BlackCodel, 10)
    , (BlackCodel, 10)
    , (BlackCodel, 10)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    ]
  , [ (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (BlackCodel, 10)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (BlackCodel, 14)
    ]
  , [ (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Cyan Light, 12)
    , (AchromaticCodel $ ChromaticColor Red Normal, 9)
    , (AchromaticCodel $ ChromaticColor Green Light, 15)
    , (BlackCodel, 16)
    , (BlackCodel, 16)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (AchromaticCodel $ ChromaticColor Magenta Light, 11)
    , (BlackCodel, 14)
    ]
  , [ (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (AchromaticCodel $ ChromaticColor Red Dark, 17)
    , (AchromaticCodel $ ChromaticColor Red Light, 18)
    , (AchromaticCodel $ ChromaticColor Red Light, 18)
    , (AchromaticCodel $ ChromaticColor Red Light, 18)
    , (BlackCodel, 19)
    , (AchromaticCodel $ ChromaticColor Green Dark, 20)
    , (AchromaticCodel $ ChromaticColor Green Dark, 20)
    , (AchromaticCodel $ ChromaticColor Red Light, 21)
    ]
  , [ (WhiteCodel, 13)
    , (AchromaticCodel $ ChromaticColor Yellow Light, 22)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (WhiteCodel, 13)
    , (AchromaticCodel $ ChromaticColor Cyan Dark, 23)
    , (AchromaticCodel $ ChromaticColor Cyan Dark, 23)
    , (WhiteCodel, 24)
    , (AchromaticCodel $ ChromaticColor Green Light, 25)
    , (AchromaticCodel $ ChromaticColor Green Light, 25)
    , (AchromaticCodel $ ChromaticColor Green Light, 25)
    , (WhiteCodel, 26)
    , (WhiteCodel, 26)
    , (WhiteCodel, 26)
    , (BlackCodel, 27)
    ]
  ]

complexBlockTable ∷ IntMap [Coordinates]
complexBlockTable = IM.fromList
  [ (0, [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)])
  , (1, [(5, 0), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (5, 2), (6, 2)])
  , (2, [(6, 0), (7, 0), (8, 0)])
  , (3, [(9, 0), (8, 1), (9, 1), (10, 1), (11, 1), (8, 2), (9, 2)])
  , (4, [(10, 0), (11, 0), (12, 0)])
  , (5, [(13, 0), (14, 0), (15, 0)])
  , (6, [(0, 1), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2), (3, 2)])
  , (7, [(12, 1), (13, 1), (14, 1), (10, 2), (11, 2), (12, 2)])
  , (8, [(15, 1)])
  , (9, [(4, 2), (7, 2), (3, 3), (4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4), (11, 4), (8, 5)])
  , (10, [(13, 2), (14, 2), (9, 3), (10, 3), (11, 3), (12, 3), (13, 3), (12, 4)])
  , (11, [(15, 2), (14, 3), (15, 3), (13, 4), (14, 4), (12, 5), (13, 5), (14, 5)])
  , (12, [(0, 3), (1, 3), (2, 3), (2, 4), (3, 4), (4, 4), (3, 5), (4, 5), (5, 5), (6, 5), (7, 5)])
  , (13, [(0, 4), (1, 4), (0, 5), (1, 5), (2, 5), (0, 6), (1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (0, 7), (2, 7), (3, 7), (4, 7), (5, 7)])
  , (14, [(15, 4), (15, 5)])
  , (15, [(9, 5)])
  , (16, [(10, 5), (11, 5)])
  , (17, [(8, 6)])
  , (18, [(9, 6), (10, 6), (11, 6)])
  , (19, [(12, 6)])
  , (20, [(13, 6), (14, 6)])
  , (21, [(15, 6)])
  , (22, [(1, 7)])
  , (23, [(6, 7), (7, 7)])
  , (24, [(8, 7)])
  , (25, [(9, 7), (10, 7), (11, 7)])
  , (26, [(12, 7), (13, 7), (14, 7)])
  , (27, [(15, 7)])
  ]

expectedComplexGraph ∷ SyntaxGraph
expectedComplexGraph = SyntaxGraph 0 rl $ IM.fromList
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
