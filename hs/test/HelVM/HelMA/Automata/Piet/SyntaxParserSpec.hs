module HelVM.HelMA.Automata.Piet.SyntaxParserSpec
  ( main
  , spec
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automata.Piet.SyntaxParser
import           HelVM.HelMA.Automata.Piet.SyntaxTestHelper
import           HelVM.HelMA.Automata.Piet.TestUtils

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Codel
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Command
import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness
import           HelVM.HelMA.Automata.Piet.Types.Matrix
import           HelVM.HelMA.Automata.Piet.Types.SyntaxGraph

import qualified Data.IntMap                                    as IM
import qualified Data.Map                                       as M
import qualified Data.Vector.Generic                            as V

import           Test.Hspec

data ImageTestCase
  = ImageTestCase
      { caseName      :: String
      , testImage     :: Matrix Codel
      , blockTable    :: IntMap BlockCoordinates
      , expectedGraph :: Maybe SyntaxGraph
      }

data ErrorTestCase
  = ErrorTestCase
      { errCaseName   :: String
      , errTestImage  :: Matrix Codel
      , errBlockTable :: IntMap BlockCoordinates
      , expectedErr   :: String
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
          res <- runIO . runSafeT $ parseFilledImage (testImage tc, blockTable tc)
          it "returns a syntax graph" $ safeToEitherLegacy res `shouldBe` Right (expectedGraph tc)

    forM_
      [ ErrorTestCase "emptyImage" V.empty IM.empty "EmptyBlockTableError\n"
      , ErrorTestCase "blackImage" blackImage blackBlockTable "IllegalInitialColorError\n"
      ] $ \tc ->
        context ("when given " ++ errCaseName tc) $ do
          res <- runIO . runSafeT $ parseFilledImage (errTestImage tc, errBlockTable tc)
          it "returns an error" $ safeToEitherLegacy res `shouldBe` Left (expectedErr tc)

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
          let image = toVector2D [[Codel (color1 tc) 0, Codel (color2 tc) 1]]
          let bTable = IM.fromList [(0, [(0, 0)]), (1, [(1, 0)])]
          let expectedG = Just $ SyntaxGraph (BlockEdge 0 rl) $
                                IM.fromList [ ( 0
                                              , Block $ M.fromList [ (rl, Just $ NextBlock (command12 tc) (BlockEdge 1 rl))
                                                                   , (rr, Just $ NextBlock (command12 tc) (BlockEdge 1 rr))
                                                                   ]
                                              )
                                            , ( 1
                                              , Block $ M.fromList [ (ll, Just $ NextBlock (command21 tc) (BlockEdge 0 ll))
                                                                   , (lr, Just $ NextBlock (command21 tc) (BlockEdge 0 lr))
                                                                   ]
                                              )
                                            ]
          res <- runIO . runSafeT $ parseFilledImage (image, bTable)
          it ("returns " ++ show (command12 tc, command21 tc) ++ " when given " ++ show (color1 tc, color2 tc)) $ safeToEitherLegacy res `shouldBe` Right expectedG


smallImage ∷ Matrix Codel
smallImage = toVector2D [[Codel (Chromatic $ ChromaticColor Red Normal) 0]]

smallBlockTable ∷ IntMap BlockCoordinates
smallBlockTable = IM.fromList [(0, [(0, 0)])]

expectedSmallGraph ∷ Maybe SyntaxGraph
expectedSmallGraph = Just $ SyntaxGraph (BlockEdge 0 rl) $ IM.fromList [(0, Block M.empty)]

whiteImage ∷ Matrix Codel
whiteImage = toVector2D [[Codel White 0]]

whiteBlockTable ∷ IntMap BlockCoordinates
whiteBlockTable = IM.fromList [(0, [(0, 0)])]

blackImage ∷ Matrix Codel
blackImage = toVector2D [[Codel Black 0]]

blackBlockTable ∷ IntMap BlockCoordinates
blackBlockTable = IM.fromList [(0, [(0, 0)])]

distantInitialImage ∷ Matrix Codel
distantInitialImage = toVector2D
  [ [ Codel White 0
    , Codel White 0
    , Codel White 0
    ]
  , [ Codel (Chromatic $ ChromaticColor Red Normal) 1
    , Codel White 0
    , Codel White 0
    ]
  , [ Codel White 0
    , Codel White 0
    , Codel White 0
    ]
  ]

distantInitialBlockTable ∷ IntMap BlockCoordinates
distantInitialBlockTable = IM.fromList
  [ (0, [(0, 0), (1, 0), (2, 0), (1, 1), (2, 1), (0, 2), (1, 2), (2, 2)])
  , (1, [(0, 1)])
  ]

expectedDistantInitialGraph ∷ Maybe SyntaxGraph
expectedDistantInitialGraph = Just $ SyntaxGraph (BlockEdge 1 ur) $ IM.fromList
  [ ( 1
    , Block $ M.fromList [ (rl, Just $ NextBlock NoOperation (BlockEdge 1 ur))
                         , (rr, Just $ NextBlock NoOperation (BlockEdge 1 ul))
                         , (dl, Just $ NextBlock NoOperation (BlockEdge 1 ul))
                         , (dr, Just $ NextBlock NoOperation (BlockEdge 1 ur))
                         , (ul, Just $ NextBlock NoOperation (BlockEdge 1 ul))
                         , (ur, Just $ NextBlock NoOperation (BlockEdge 1 ur))
                         ]
    )
  ]

stuckImage ∷ Matrix Codel
stuckImage = toVector2D
  [ [ Codel (Chromatic $ ChromaticColor Red Light) 0
    , Codel (Chromatic $ ChromaticColor Red Normal) 1
    , Codel White 2
    ]
  , [ Codel White 2
    , Codel White 2
    , Codel White 2
    ]
  , [ Codel White 2
    , Codel Black 3
    , Codel White 2
    ]
  ]

stuckBlockTable ∷ IntMap BlockCoordinates
stuckBlockTable = IM.fromList
  [ (0, [(0, 0)])
  , (1, [(1, 0)])
  , (2, [(2, 0), (0, 1), (1, 1), (2, 1), (0, 2), (2, 2)])
  , (3, [(1, 2)])
  ]

expectedStuckGraph ∷ Maybe SyntaxGraph
expectedStuckGraph = Just $ SyntaxGraph (BlockEdge 0 rl) $ IM.fromList
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

rawComplexImage ∷ Matrix Color
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

complexImage ∷ Matrix Codel
complexImage = toVector2D
  [ [ Codel (Chromatic $ ChromaticColor Blue Dark) 0
    , Codel (Chromatic $ ChromaticColor Blue Dark) 0
    , Codel (Chromatic $ ChromaticColor Blue Dark) 0
    , Codel (Chromatic $ ChromaticColor Blue Dark) 0
    , Codel (Chromatic $ ChromaticColor Blue Dark) 0
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Red Light) 2
    , Codel (Chromatic $ ChromaticColor Red Light) 2
    , Codel (Chromatic $ ChromaticColor Red Light) 2
    , Codel White 3
    , Codel (Chromatic $ ChromaticColor Red Light) 4
    , Codel (Chromatic $ ChromaticColor Red Light) 4
    , Codel (Chromatic $ ChromaticColor Red Light) 4
    , Codel (Chromatic $ ChromaticColor Magenta Dark) 5
    , Codel (Chromatic $ ChromaticColor Magenta Dark) 5
    , Codel (Chromatic $ ChromaticColor Magenta Dark) 5
    ]
  , [ Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel White 3
    , Codel White 3
    , Codel White 3
    , Codel White 3
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel Black 8
    ]
  , [ Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Blue Light) 6
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Blue Normal) 1
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel White 3
    , Codel White 3
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel (Chromatic $ ChromaticColor Yellow Normal) 7
    , Codel Black 10
    , Codel Black 10
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    ]
  , [ Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel Black 10
    , Codel Black 10
    , Codel Black 10
    , Codel Black 10
    , Codel Black 10
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    ]
  , [ Codel White 13
    , Codel White 13
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel Black 10
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel Black 14
    ]
  , [ Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Cyan Light) 12
    , Codel (Chromatic $ ChromaticColor Red Normal) 9
    , Codel (Chromatic $ ChromaticColor Green Light) 15
    , Codel Black 16
    , Codel Black 16
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel (Chromatic $ ChromaticColor Magenta Light) 11
    , Codel Black 14
    ]
  , [ Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel (Chromatic $ ChromaticColor Red Dark) 17
    , Codel (Chromatic $ ChromaticColor Red Light) 18
    , Codel (Chromatic $ ChromaticColor Red Light) 18
    , Codel (Chromatic $ ChromaticColor Red Light) 18
    , Codel Black 19
    , Codel (Chromatic $ ChromaticColor Green Dark) 20
    , Codel (Chromatic $ ChromaticColor Green Dark) 20
    , Codel (Chromatic $ ChromaticColor Red Light) 21
    ]
  , [ Codel White 13
    , Codel (Chromatic $ ChromaticColor Yellow Light) 22
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel White 13
    , Codel (Chromatic $ ChromaticColor Cyan Dark) 23
    , Codel (Chromatic $ ChromaticColor Cyan Dark) 23
    , Codel White 24
    , Codel (Chromatic $ ChromaticColor Green Light) 25
    , Codel (Chromatic $ ChromaticColor Green Light) 25
    , Codel (Chromatic $ ChromaticColor Green Light) 25
    , Codel White 26
    , Codel White 26
    , Codel White 26
    , Codel Black 27
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

expectedComplexGraph ∷ Maybe SyntaxGraph
expectedComplexGraph = Just $ SyntaxGraph (BlockEdge 0 rl) $ IM.fromList
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
