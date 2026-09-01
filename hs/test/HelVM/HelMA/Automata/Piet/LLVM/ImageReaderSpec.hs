module HelVM.HelMA.Automata.Piet.LLVM.ImageReaderSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils

import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Data.Vector                                ( Vector )
import           Test.Hspec

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  pass
  describe "readCodels" $ do
    forM_
      [ ( ImageConfig { additionalColor = AdditionalColorAsBlack
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = CodelSize 5
                      }
        , blackWhiteCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAsBlack
                      , codelSize = CodelSize 5
                      }
        , whiteBlackCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelCenter
                      , codelSize = CodelSize 5
                      }
        , whiteCenterCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelModal
                      , codelSize = CodelSize 5
                      }
        , whiteModalCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAverage
                      , codelSize = CodelSize 5
                      }
        , whiteAverageCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorNearest
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = CodelSize 5
                      }
        , nearestWhiteCodels
        )
      ] $ \(config, expectedCodels) ->
        context ("when configured with " ++ show config) $ do
          res <- runIO $ runExceptT $ readCodels config "test/resources/imagereader-test.png"
          it "returns codels" $ res `shouldBe` Right expectedCodels

    context "when given GuessCodelSize" $ do
      let config = ImageConfig { additionalColor = AdditionalColorNearest
                               , multicoloredCodel = MulticoloredCodelAverage
                               , codelSize = GuessCodelSize
                               }
      res <- runIO $ runExceptT $ readCodels config "test/resources/codel10-test.png"
      it "returns codels" $ res `shouldBe` Right complexCodels

    context "when given an invalid codel size" $ do
      let config = ImageConfig { additionalColor = AdditionalColorNearest
                               , multicoloredCodel = MulticoloredCodelAverage
                               , codelSize = CodelSize 4
                               }
      res <- runIO $ runExceptT $ readCodels config "test/resources/imagereader-test.png"
      it "fails with CodelSizeError" $ res `shouldBe` Left CodelSizeError

blackWhiteCodels ∷ Vector (Vector Codel)
blackWhiteCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

whiteBlackCodels ∷ Vector (Vector Codel)
whiteBlackCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, WhiteCodel, WhiteCodel]
  ]

whiteCenterCodels ∷ Vector (Vector Codel)
whiteCenterCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel Green Normal, AchromaticCodel Blue Normal, WhiteCodel, AchromaticCodel Red Normal, WhiteCodel, WhiteCodel]
  ]

whiteModalCodels ∷ Vector (Vector Codel)
whiteModalCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, WhiteCodel, AchromaticCodel Red Normal, WhiteCodel, WhiteCodel]
  ]

whiteAverageCodels ∷ Vector (Vector Codel)
whiteAverageCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, AchromaticCodel Magenta Dark, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

nearestWhiteCodels ∷ Vector (Vector Codel)
nearestWhiteCodels = toVector2D
  [ [AchromaticCodel Red Light, AchromaticCodel Yellow Light, AchromaticCodel Green Light, AchromaticCodel Cyan Light, AchromaticCodel Blue Light, AchromaticCodel Magenta Light]
  , [AchromaticCodel Red Normal, AchromaticCodel Yellow Normal, AchromaticCodel Green Normal, AchromaticCodel Cyan Normal, AchromaticCodel Blue Normal, AchromaticCodel Magenta Normal]
  , [AchromaticCodel Red Dark, AchromaticCodel Yellow Dark, AchromaticCodel Green Dark, AchromaticCodel Cyan Dark, AchromaticCodel Blue Dark, AchromaticCodel Magenta Dark]
  , [BlackCodel, AchromaticCodel Red Light, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel Red Normal, AchromaticCodel Red Dark, AchromaticCodel Green Dark, BlackCodel, WhiteCodel, AchromaticCodel Blue Light]
  , [AchromaticCodel Yellow Normal, AchromaticCodel Cyan Normal, AchromaticCodel Magenta Normal, AchromaticCodel Magenta Normal, AchromaticCodel Yellow Normal, AchromaticCodel Cyan Normal]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

complexCodels ∷ Vector (Vector Codel)
complexCodels = toVector2D
  [ [ AchromaticCodel Blue Dark
    , AchromaticCodel Blue Dark
    , AchromaticCodel Blue Dark
    , AchromaticCodel Blue Dark
    , AchromaticCodel Blue Dark
    , AchromaticCodel Blue Normal
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , WhiteCodel
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , AchromaticCodel Magenta Dark
    , AchromaticCodel Magenta Dark
    , AchromaticCodel Magenta Dark
    ]
  , [ AchromaticCodel Blue Light
    , AchromaticCodel Blue Light
    , AchromaticCodel Blue Light
    , AchromaticCodel Blue Normal
    , AchromaticCodel Blue Normal
    , AchromaticCodel Blue Normal
    , AchromaticCodel Blue Normal
    , AchromaticCodel Blue Normal
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel Yellow Normal
    , AchromaticCodel Yellow Normal
    , AchromaticCodel Yellow Normal
    , BlackCodel
    ]
  , [ AchromaticCodel Blue Light
    , AchromaticCodel Blue Light
    , AchromaticCodel Blue Light
    , AchromaticCodel Blue Light
    , AchromaticCodel Red Normal
    , AchromaticCodel Blue Normal
    , AchromaticCodel Blue Normal
    , AchromaticCodel Red Normal
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel Yellow Normal
    , AchromaticCodel Yellow Normal
    , AchromaticCodel Yellow Normal
    , BlackCodel
    , BlackCodel
    , AchromaticCodel Magenta Light
    ]
  , [ AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , BlackCodel
    , AchromaticCodel Magenta Light
    , AchromaticCodel Magenta Light
    ]
  , [ WhiteCodel
    , WhiteCodel
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , AchromaticCodel Red Normal
    , BlackCodel
    , AchromaticCodel Magenta Light
    , AchromaticCodel Magenta Light
    , BlackCodel
    ]
  , [ WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Cyan Light
    , AchromaticCodel Red Normal
    , AchromaticCodel Green Light
    , BlackCodel
    , BlackCodel
    , AchromaticCodel Magenta Light
    , AchromaticCodel Magenta Light
    , AchromaticCodel Magenta Light
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
    , AchromaticCodel Red Dark
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , AchromaticCodel Red Light
    , BlackCodel
    , AchromaticCodel Green Dark
    , AchromaticCodel Green Dark
    , AchromaticCodel Red Light
    ]
  , [ WhiteCodel
    , AchromaticCodel Yellow Light
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , AchromaticCodel Cyan Dark
    , AchromaticCodel Cyan Dark
    , WhiteCodel
    , AchromaticCodel Green Light
    , AchromaticCodel Green Light
    , AchromaticCodel Green Light
    , WhiteCodel
    , WhiteCodel
    , WhiteCodel
    , BlackCodel
    ]
  ]
