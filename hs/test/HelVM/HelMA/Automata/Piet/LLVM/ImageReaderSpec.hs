module HelVM.HelMA.Automata.Piet.LLVM.ImageReaderSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.Codel
import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness

import           Data.Vector                                    ( Vector )
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
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel, BlackCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

whiteBlackCodels ∷ Vector (Vector Codel)
whiteBlackCodels = toVector2D
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [BlackCodel, BlackCodel, BlackCodel, BlackCodel, WhiteCodel, WhiteCodel]
  ]

whiteCenterCodels ∷ Vector (Vector Codel)
whiteCenterCodels = toVector2D
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Blue Normal, WhiteCodel, AchromaticCodel $ ChromaticColor Red Normal, WhiteCodel, WhiteCodel]
  ]

whiteModalCodels ∷ Vector (Vector Codel)
whiteModalCodels = toVector2D
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, WhiteCodel, AchromaticCodel $ ChromaticColor Red Normal, WhiteCodel, WhiteCodel]
  ]

whiteAverageCodels ∷ Vector (Vector Codel)
whiteAverageCodels = toVector2D
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [WhiteCodel, WhiteCodel, AchromaticCodel $ ChromaticColor Magenta Dark, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

nearestWhiteCodels ∷ Vector (Vector Codel)
nearestWhiteCodels = toVector2D
  [ [AchromaticCodel $ ChromaticColor Red Light, AchromaticCodel $ ChromaticColor Yellow Light, AchromaticCodel $ ChromaticColor Green Light, AchromaticCodel $ ChromaticColor Cyan Light, AchromaticCodel $ ChromaticColor Blue Light, AchromaticCodel $ ChromaticColor Magenta Light]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Green Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Blue Normal, AchromaticCodel $ ChromaticColor Magenta Normal]
  , [AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Yellow Dark, AchromaticCodel $ ChromaticColor Green Dark, AchromaticCodel $ ChromaticColor Cyan Dark, AchromaticCodel $ ChromaticColor Blue Dark, AchromaticCodel $ ChromaticColor Magenta Dark]
  , [BlackCodel, AchromaticCodel $ ChromaticColor Red Light, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  , [AchromaticCodel $ ChromaticColor Red Normal, AchromaticCodel $ ChromaticColor Red Dark, AchromaticCodel $ ChromaticColor Green Dark, BlackCodel, WhiteCodel, AchromaticCodel $ ChromaticColor Blue Light]
  , [AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Cyan Normal, AchromaticCodel $ ChromaticColor Magenta Normal, AchromaticCodel $ ChromaticColor Magenta Normal, AchromaticCodel $ ChromaticColor Yellow Normal, AchromaticCodel $ ChromaticColor Cyan Normal]
  , [WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel, WhiteCodel]
  ]

complexCodels ∷ Vector (Vector Codel)
complexCodels = toVector2D
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
