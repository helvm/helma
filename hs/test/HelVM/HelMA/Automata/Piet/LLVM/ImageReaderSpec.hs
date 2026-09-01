module HelVM.HelMA.Automata.Piet.LLVM.ImageReaderSpec
  ( main
  , spec
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.ImageReader
import           HelVM.HelMA.Automata.Piet.LLVM.TestUtils
import           HelVM.HelMA.Automata.Piet.Types.Color

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

blackWhiteCodels ∷ Vector (Vector Color)
blackWhiteCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, Black, White, White, White, White]
  , [Black, Black, Black, Black, Black, Black]
  , [Black, Black, Black, Black, Black, Black]
  , [White, White, White, White, White, White]
  ]

whiteBlackCodels ∷ Vector (Vector Color)
whiteBlackCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Black, Black, Black, Black, White, White]
  ]

whiteCenterCodels ∷ Vector (Vector Color)
whiteCenterCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Blue Normal, White, Chromatic $ ChromaticColor Red Normal, White, White]
  ]

whiteModalCodels ∷ Vector (Vector Color)
whiteModalCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, White, Chromatic $ ChromaticColor Red Normal, White, White]
  ]

whiteAverageCodels ∷ Vector (Vector Color)
whiteAverageCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, Chromatic $ ChromaticColor Magenta Dark, White, White, White]
  ]

nearestWhiteCodels ∷ Vector (Vector Color)
nearestWhiteCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, Chromatic $ ChromaticColor Red Light, White, White, White, White]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Green Dark, Black, White, Chromatic $ ChromaticColor Blue Light]
  , [Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Magenta Normal, Chromatic $ ChromaticColor Magenta Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Cyan Normal]
  , [White, White, White, White, White, White]
  ]

complexCodels ∷ Vector (Vector Color)
complexCodels = toVector2D
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
