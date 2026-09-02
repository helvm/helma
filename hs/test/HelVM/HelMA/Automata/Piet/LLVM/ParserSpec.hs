module HelVM.HelMA.Automata.Piet.LLVM.ParserSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Parser
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxGraph
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils
import           HelVM.HelMA.Automata.Piet.Types.Color

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Command
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
      , testImage     :: Vector (Vector (Color, Int))
      , blockTable    :: IntMap BlockCoordinates
      , expectedGraph :: SyntaxGraphMaybe
      }

data ErrorTestCase
  = ErrorTestCase
      { errCaseName   :: String
      , errTestImage  :: Vector (Vector (Color, Int))
      , errBlockTable :: IntMap BlockCoordinates
      , expectedErr   :: ParserError
      }

data TwoPixelTestCase
  = TwoPixelTestCase
      { color1    :: Color
      , color2    :: Color
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
      , ImageTestCase "whiteImage" whiteImage whiteBlockTable Nothing
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
        [ TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Red Normal) (Push 1) Pop
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Red Dark) Pop (Push 1)
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Yellow Light) Add InChar
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Yellow Normal) Subtract OutChar
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Yellow Dark) Multiply OutNumber
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Green Light) Divide Duplicate
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Green Normal) Mod InNumber
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Green Dark) Not Roll
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Cyan Light) Greater Greater
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Cyan Normal) Pointer Switch
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Cyan Dark) Switch Pointer
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Blue Light) Duplicate Divide
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Blue Normal) Roll Not
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Blue Dark) InNumber Mod
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Magenta Light) InChar Add
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Magenta Normal) OutNumber Multiply
        , TwoPixelTestCase (Chromatic $ ChromaticColor Red Light) (Chromatic $ ChromaticColor Magenta Dark) OutChar Subtract
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Cyan Light) (Push 1) Pop
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Cyan Normal) Pop (Push 1)
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Blue Dark) Add InChar
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Blue Light) Subtract OutChar
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Blue Normal) Multiply OutNumber
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Magenta Dark) Divide Duplicate
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Magenta Light) Mod InNumber
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Magenta Normal) Not Roll
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Red Dark) Greater Greater
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Red Light) Pointer Switch
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Red Normal) Switch Pointer
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Yellow Dark) Duplicate Divide
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Yellow Light) Roll Not
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Yellow Normal) InNumber Mod
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Green Dark) InChar Add
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Green Light) OutNumber Multiply
        , TwoPixelTestCase (Chromatic $ ChromaticColor Cyan Dark) (Chromatic $ ChromaticColor Green Normal) OutChar Subtract
        ] $ \tc -> do
          let image = toVector2D [[(color1 tc, 0), (color2 tc, 1)]]
          let bTable = IM.fromList [(0, [(0, 0)]), (1, [(1, 0)])]
          let expectedG = Just $ SyntaxGraph 0 rl $
                                IM.fromList [ ( 0
                                              , Block $ M.fromList [ (rl, Just $ NextBlock (command12 tc) rl 1)
                                                                   , (rr, Just $ NextBlock (command12 tc) rr 1)
                                                                   ]
                                              )
                                            , ( 1
                                              , Block $ M.fromList [ (ll, Just $ NextBlock (command21 tc) ll 0)
                                                                   , (lr, Just $ NextBlock (command21 tc) lr 0)
                                                                   ]
                                              )
                                            ]
          it ("returns " ++ show (command12 tc, command21 tc) ++ " when given " ++ show (color1 tc, color2 tc)) $ parseFilledImage (image, bTable) `shouldBe` Right expectedG


smallImage ∷ Vector (Vector (Color, Int))
smallImage = toVector2D [[(Chromatic $ ChromaticColor Red Normal, 0)]]

smallBlockTable ∷ IntMap BlockCoordinates
smallBlockTable = IM.fromList [(0, [(0, 0)])]

expectedSmallGraph ∷ SyntaxGraphMaybe
expectedSmallGraph = Just $ SyntaxGraph 0 rl $ IM.fromList [(0, Block M.empty)]

whiteImage ∷ Vector (Vector (Color, Int))
whiteImage = toVector2D [[(White, 0)]]

whiteBlockTable ∷ IntMap BlockCoordinates
whiteBlockTable = IM.fromList [(0, [(0, 0)])]

blackImage ∷ Vector (Vector (Color, Int))
blackImage = toVector2D [[(Black, 0)]]

blackBlockTable ∷ IntMap BlockCoordinates
blackBlockTable = IM.fromList [(0, [(0, 0)])]

distantInitialImage ∷ Vector (Vector (Color, Int))
distantInitialImage = toVector2D
  [ [ (White, 0)
    , (White, 0)
    , (White, 0)
    ]
  , [ (Chromatic $ ChromaticColor Red Normal, 1)
    , (White, 0)
    , (White, 0)
    ]
  , [ (White, 0)
    , (White, 0)
    , (White, 0)
    ]
  ]

distantInitialBlockTable ∷ IntMap BlockCoordinates
distantInitialBlockTable = IM.fromList
  [ (0, [(0, 0), (1, 0), (2, 0), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)])
  , (1, [(0, 1)])
  ]

expectedDistantInitialGraph ∷ SyntaxGraphMaybe
expectedDistantInitialGraph = Just $ SyntaxGraph 1 ur $ IM.fromList
  [ ( 1
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation ur 1)
                         , (rr, Just $ NextBlock NoOperation ul 1)
                         , (dl, Just $ NextBlock NoOperation ul 1)
                         , (dr, Just $ NextBlock NoOperation ur 1)
                         , (ul, Just $ NextBlock NoOperation ul 1)
                         , (ur, Just $ NextBlock NoOperation ur 1)
                         ]
    )
  ]

stuckImage ∷ Vector (Vector (Color, Int))
stuckImage = toVector2D
  [ [ (Chromatic $ ChromaticColor Red Light, 0)
    , (Chromatic $ ChromaticColor Red Normal, 1)
    , (White, 2)
    ]
  , [ (White, 2)
    , (White, 2)
    , (White, 2)
    ]
  , [ (White, 2)
    , (Black, 3)
    , (White, 2)
    ]
  ]

stuckBlockTable ∷ IntMap BlockCoordinates
stuckBlockTable = IM.fromList
  [ (0, [(0, 0)])
  , (1, [(1, 0)])
  , (2, [(2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (2, 2)])
  , (3, [(1, 2)])
  ]

expectedStuckGraph ∷ SyntaxGraphMaybe
expectedStuckGraph = Just $ SyntaxGraph 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, Just $ NextBlock (Push 1) rl 1)
                         , (rr, Just $ NextBlock (Push 1) rr 1)
                         , (dl, Just $ NextBlock NoOperation ul 0)
                         , (dr, Just $ NextBlock NoOperation ur 0)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, Nothing)
                         , (rr, Nothing)
                         , (dl, Just $ NextBlock NoOperation ul 0)
                         , (dr, Just $ NextBlock NoOperation ur 0)
                         , (ll, Just $ NextBlock Pop ll 0)
                         , (lr, Just $ NextBlock Pop lr 0)
                         ]
    )
  ]

rawComplexImage ∷ Vector (Vector Color)
rawComplexImage = toVector2D
  [ [ Chromatic $ ChromaticColor Blue Dark
    , Chromatic $ ChromaticColor Blue Dark
    , Chromatic $ ChromaticColor Blue Dark
    , Chromatic $ ChromaticColor Blue Dark
    , Chromatic $ ChromaticColor Blue Dark
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , White
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Magenta Dark
    , Chromatic $ ChromaticColor Magenta Dark
    , Chromatic $ ChromaticColor Magenta Dark
    ]
  , [ Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Blue Normal
    , White
    , White
    , White
    , White
    , Chromatic $ ChromaticColor Yellow Normal
    , Chromatic $ ChromaticColor Yellow Normal
    , Chromatic $ ChromaticColor Yellow Normal
    , Black
    ]
  , [ Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Blue Light
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Blue Normal
    , Chromatic $ ChromaticColor Red Normal
    , White
    , White
    , Chromatic $ ChromaticColor Yellow Normal
    , Chromatic $ ChromaticColor Yellow Normal
    , Chromatic $ ChromaticColor Yellow Normal
    , Black
    , Black
    , Chromatic $ ChromaticColor Magenta Light
    ]
  , [ Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Black
    , Black
    , Black
    , Black
    , Black
    , Chromatic $ ChromaticColor Magenta Light
    , Chromatic $ ChromaticColor Magenta Light
    ]
  , [ White
    , White
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Red Normal
    , Black
    , Chromatic $ ChromaticColor Magenta Light
    , Chromatic $ ChromaticColor Magenta Light
    , Black
    ]
  , [ White
    , White
    , White
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Cyan Light
    , Chromatic $ ChromaticColor Red Normal
    , Chromatic $ ChromaticColor Green Light
    , Black
    , Black
    , Chromatic $ ChromaticColor Magenta Light
    , Chromatic $ ChromaticColor Magenta Light
    , Chromatic $ ChromaticColor Magenta Light
    , Black
    ]
  , [ White
    , White
    , White
    , White
    , White
    , White
    , White
    , White
    , Chromatic $ ChromaticColor Red Dark
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , Chromatic $ ChromaticColor Red Light
    , Black
    , Chromatic $ ChromaticColor Green Dark
    , Chromatic $ ChromaticColor Green Dark
    , Chromatic $ ChromaticColor Red Light
    ]
  , [ White
    , Chromatic $ ChromaticColor Yellow Light
    , White
    , White
    , White
    , White
    , Chromatic $ ChromaticColor Cyan Dark
    , Chromatic $ ChromaticColor Cyan Dark
    , White
    , Chromatic $ ChromaticColor Green Light
    , Chromatic $ ChromaticColor Green Light
    , Chromatic $ ChromaticColor Green Light
    , White
    , White
    , White
    , Black
    ]
  ]

complexImage ∷ Vector (Vector (Color, Int))
complexImage = toVector2D
  [ [ (Chromatic $ ChromaticColor Blue Dark, 0)
    , (Chromatic $ ChromaticColor Blue Dark, 0)
    , (Chromatic $ ChromaticColor Blue Dark, 0)
    , (Chromatic $ ChromaticColor Blue Dark, 0)
    , (Chromatic $ ChromaticColor Blue Dark, 0)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Red Light, 2)
    , (Chromatic $ ChromaticColor Red Light, 2)
    , (Chromatic $ ChromaticColor Red Light, 2)
    , (White, 3)
    , (Chromatic $ ChromaticColor Red Light, 4)
    , (Chromatic $ ChromaticColor Red Light, 4)
    , (Chromatic $ ChromaticColor Red Light, 4)
    , (Chromatic $ ChromaticColor Magenta Dark, 5)
    , (Chromatic $ ChromaticColor Magenta Dark, 5)
    , (Chromatic $ ChromaticColor Magenta Dark, 5)
    ]
  , [ (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (White, 3)
    , (White, 3)
    , (White, 3)
    , (White, 3)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Black, 8)
    ]
  , [ (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Blue Light, 6)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Blue Normal, 1)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (White, 3)
    , (White, 3)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Chromatic $ ChromaticColor Yellow Normal, 7)
    , (Black, 10)
    , (Black, 10)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    ]
  , [ (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Black, 10)
    , (Black, 10)
    , (Black, 10)
    , (Black, 10)
    , (Black, 10)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    ]
  , [ (White, 13)
    , (White, 13)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Black, 10)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Black, 14)
    ]
  , [ (White, 13)
    , (White, 13)
    , (White, 13)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Cyan Light, 12)
    , (Chromatic $ ChromaticColor Red Normal, 9)
    , (Chromatic $ ChromaticColor Green Light, 15)
    , (Black, 16)
    , (Black, 16)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Chromatic $ ChromaticColor Magenta Light, 11)
    , (Black, 14)
    ]
  , [ (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (Chromatic $ ChromaticColor Red Dark, 17)
    , (Chromatic $ ChromaticColor Red Light, 18)
    , (Chromatic $ ChromaticColor Red Light, 18)
    , (Chromatic $ ChromaticColor Red Light, 18)
    , (Black, 19)
    , (Chromatic $ ChromaticColor Green Dark, 20)
    , (Chromatic $ ChromaticColor Green Dark, 20)
    , (Chromatic $ ChromaticColor Red Light, 21)
    ]
  , [ (White, 13)
    , (Chromatic $ ChromaticColor Yellow Light, 22)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (White, 13)
    , (Chromatic $ ChromaticColor Cyan Dark, 23)
    , (Chromatic $ ChromaticColor Cyan Dark, 23)
    , (White, 24)
    , (Chromatic $ ChromaticColor Green Light, 25)
    , (Chromatic $ ChromaticColor Green Light, 25)
    , (Chromatic $ ChromaticColor Green Light, 25)
    , (White, 26)
    , (White, 26)
    , (White, 26)
    , (Black, 27)
    ]
  ]

complexBlockTable ∷ IntMap BlockCoordinates
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

expectedComplexGraph ∷ SyntaxGraphMaybe
expectedComplexGraph = Just $ SyntaxGraph 0 rl $ IM.fromList
  [ ( 0
    , Block $ M.fromList [ (rl, Just $ NextBlock Pop rl 1)
                         , (rr, Just $ NextBlock Pop rr 1)
                         , (dl, Just $ NextBlock Pop dl 1)
                         , (dr, Just $ NextBlock (Push 5) dr 6)
                         ]
    )
  , ( 1
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation rl 7)
                         , (rr, Just $ NextBlock NoOperation rr 7)
                         , (dl, Just $ NextBlock Divide dl 9)
                         , (dr, Just $ NextBlock Divide dr 9)
                         , (ll, Just $ NextBlock Pop ll 6)
                         , (lr, Just $ NextBlock Pop lr 6)
                         ]
    )
  , ( 2
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation rl 4)
                         , (rr, Just $ NextBlock NoOperation rr 4)
                         , (dl, Just $ NextBlock NoOperation dl 9)
                         , (dr, Just $ NextBlock Roll dr 1)
                         , (ll, Just $ NextBlock Roll ll 1)
                         , (lr, Just $ NextBlock Roll lr 1)
                         ]
    )
  , ( 4
    , Block $ M.fromList [ (rl, Just $ NextBlock OutChar rl 5)
                         , (rr, Just $ NextBlock OutChar rr 5)
                         , (dl, Just $ NextBlock Subtract dl 7)
                         , (dr, Just $ NextBlock NoOperation dr 7)
                         , (ll, Just $ NextBlock NoOperation ll 2)
                         , (lr, Just $ NextBlock NoOperation lr 2)
                         ]
    )
  , ( 5
    , Block $ M.fromList [ (dr, Just $ NextBlock Not dr 7)
                         , (ll, Just $ NextBlock Subtract ll 4)
                         , (lr, Just $ NextBlock Subtract lr 4)
                         ]
    )
  , ( 6
    , Block $ M.fromList [ (rl, Just $ NextBlock Mod rl 9)
                         , (rr, Just $ NextBlock Mod rr 9)
                         , (dl, Just $ NextBlock Mod dl 9)
                         , (dr, Just $ NextBlock InChar dr 12)
                         , (ul, Just $ NextBlock Pop ul 0)
                         , (ur, Just $ NextBlock Pop ur 0)
                         ]
    )
  , ( 7
    , Block $ M.fromList [ (ll, Just $ NextBlock NoOperation ll 9)
                         , (lr, Just $ NextBlock NoOperation lr 9)
                         , (ul, Just $ NextBlock OutChar ul 4)
                         , (ur, Just $ NextBlock Roll ur 5)
                         ]
    )
  , ( 9
    , Block $ M.fromList [ (dl, Just $ NextBlock (Push 16) dl 17)
                         , (dr, Just $ NextBlock (Push 16) dr 17)
                         , (ll, Just $ NextBlock Switch ll 12)
                         , (lr, Just $ NextBlock Switch lr 12)
                         , (ul, Just $ NextBlock Duplicate ul 1)
                         , (ur, Just $ NextBlock Duplicate ur 1)
                         ]
    )
  , ( 12
    , Block $ M.fromList [ (rl, Just $ NextBlock Pointer rl 9)
                         , (rr, Just $ NextBlock Pointer rr 9)
                         , (dl, Just $ NextBlock NoOperation dl 23)
                         , (dr, Just $ NextBlock NoOperation ll 22)
                         , (ul, Just $ NextBlock Add ul 6)
                         , (ur, Just $ NextBlock Add ur 6)
                         ]
    )
  , ( 15
    , Block $ M.fromList [ (dl, Just $ NextBlock Duplicate dl 18)
                         , (dr, Just $ NextBlock Duplicate dr 18)
                         , (ll, Just $ NextBlock Roll ll 9)
                         , (lr, Just $ NextBlock Roll lr 9)
                         , (ul, Just $ NextBlock Roll ul 9)
                         , (ur, Just $ NextBlock Roll ur 9)
                         ]
    )
  , ( 17
    , Block $ M.fromList [ (rl, Just $ NextBlock (Push 1) rl 18)
                         , (rr, Just $ NextBlock (Push 1) rr 18)
                         , (dl, Just $ NextBlock NoOperation lr 23)
                         , (dr, Just $ NextBlock NoOperation ll 23)
                         , (ll, Just $ NextBlock NoOperation ur 12)
                         , (lr, Just $ NextBlock NoOperation ul 12)
                         , (ul, Just $ NextBlock Pop ul 9)
                         , (ur, Just $ NextBlock Pop ur 9)
                         ]
    )
  , ( 18
    , Block $ M.fromList [ (dl, Just $ NextBlock Divide dl 25)
                         , (dr, Just $ NextBlock Divide dr 25)
                         , (ll, Just $ NextBlock Pop ll 17)
                         , (lr, Just $ NextBlock Pop lr 17)
                         , (ul, Just $ NextBlock Divide ul 15)
                         ]
    )
  , ( 22
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation rl 23)
                         , (rr, Just $ NextBlock NoOperation rr 23)
                         , (ll, Just $ NextBlock NoOperation ur 12)
                         , (lr, Just $ NextBlock NoOperation ul 12)
                         , (ul, Just $ NextBlock NoOperation ul 12)
                         , (ur, Just $ NextBlock NoOperation ur 12)
                         ]
    )
  , ( 23
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation rl 25)
                         , (rr, Just $ NextBlock NoOperation rr 25)
                         , (ll, Just $ NextBlock NoOperation ll 22)
                         , (lr, Just $ NextBlock NoOperation lr 22)
                         , (ul, Just $ NextBlock NoOperation ul 12)
                         , (ur, Just $ NextBlock NoOperation ur 12)
                         ]
    )
  , ( 25
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation ll 25)
                         , (rr, Just $ NextBlock NoOperation lr 25)
                         , (ll, Just $ NextBlock NoOperation ll 23)
                         , (lr, Just $ NextBlock NoOperation lr 23)
                         , (ul, Just $ NextBlock Duplicate ul 18)
                         , (ur, Just $ NextBlock Duplicate ur 18)
                         ]
    )
  ]
