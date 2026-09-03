module HelVM.HelMA.Automata.Piet.ImageReaderSpec
  ( main
  , spec
  ) where

import           HelVM.HelIO.Control.Safe

import           HelVM.HelMA.Automata.Piet.ImageReader
import           HelVM.HelMA.Automata.Piet.TestUtils

import           HelVM.HelMA.Automata.Piet.API.AdditionalColorStrategy
import           HelVM.HelMA.Automata.Piet.API.ImageConfig
import           HelVM.HelMA.Automata.Piet.API.MulticoloredCodelStrategy

import           HelVM.HelMA.Automata.Piet.Types.ChromaticColor
import           HelVM.HelMA.Automata.Piet.Types.Color
import           HelVM.HelMA.Automata.Piet.Types.Hue
import           HelVM.HelMA.Automata.Piet.Types.Lightness
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Codec.Picture
import           Test.Hspec

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  pass
  describe "readColors" $ do
    forM_
      [ ( ImageConfig { additionalColor = AdditionalColorAsBlack
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = Just 5
                      }
        , blackWhiteCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAsBlack
                      , codelSize = Just 5
                      }
        , whiteBlackCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelCenter
                      , codelSize = Just 5
                      }
        , whiteCenterCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelModal
                      , codelSize = Just 5
                      }
        , whiteModalCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorAsWhite
                      , multicoloredCodel = MulticoloredCodelAverage
                      , codelSize = Just 5
                      }
        , whiteAverageCodels
        )
      , ( ImageConfig { additionalColor = AdditionalColorNearest
                      , multicoloredCodel = MulticoloredCodelAsWhite
                      , codelSize = Just 5
                      }
        , nearestWhiteCodels
        )
      ] $ \(config, expectedCodels) ->
        context ("when configured with " ++ show config) $ do
          res <- runIO . runSafeT $ readColors config =<< readImageFile "test/resources/imagereader-test.png"
          it "returns codels" $ safeToEitherLegacy res `shouldBe` Right expectedCodels

    context "when given Nothing" $ do
      let config = ImageConfig { additionalColor = AdditionalColorNearest
                               , multicoloredCodel = MulticoloredCodelAverage
                               , codelSize = Nothing
                               }
      res <- runIO . runSafeT $ readColors config =<< readImageFile "test/resources/codel10-test.png"
      it "returns codels" $ safeToEitherLegacy res `shouldBe` Right complexCodels

    context "when given an invalid codel size" $ do
      let config = ImageConfig { additionalColor = AdditionalColorNearest
                               , multicoloredCodel = MulticoloredCodelAverage
                               , codelSize = Just 4
                               }
      res <- runIO . runSafeT $ readColors config =<< readImageFile "test/resources/imagereader-test.png"
      it "fails with CodelSizeError" $ do
        let leftText = safeToEitherLegacy res
        leftText `shouldBe` Left "CodelSizeError\n"

readImageFile ∷ MonadIO m ⇒ FilePath → m DynamicImage
readImageFile filePath = either (error . show) pure =<< liftIO (readImage filePath)

blackWhiteCodels ∷ Matrix Color
blackWhiteCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, Black, White, White, White, White]
  , [Black, Black, Black, Black, Black, Black]
  , [Black, Black, Black, Black, Black, Black]
  , [White, White, White, White, White, White]
  ]

whiteBlackCodels ∷ Matrix Color
whiteBlackCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Black, Black, Black, Black, White, White]
  ]

whiteCenterCodels ∷ Matrix Color
whiteCenterCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Blue Normal, White, Chromatic $ ChromaticColor Red Normal, White, White]
  ]

whiteModalCodels ∷ Matrix Color
whiteModalCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, White, Chromatic $ ChromaticColor Red Normal, White, White]
  ]

whiteAverageCodels ∷ Matrix Color
whiteAverageCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, White, White, White, White]
  , [White, White, Chromatic $ ChromaticColor Magenta Dark, White, White, White]
  ]

nearestWhiteCodels ∷ Matrix Color
nearestWhiteCodels = toVector2D
  [ [Chromatic $ ChromaticColor Red Light, Chromatic $ ChromaticColor Yellow Light, Chromatic $ ChromaticColor Green Light, Chromatic $ ChromaticColor Cyan Light, Chromatic $ ChromaticColor Blue Light, Chromatic $ ChromaticColor Magenta Light]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Green Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Blue Normal, Chromatic $ ChromaticColor Magenta Normal]
  , [Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Yellow Dark, Chromatic $ ChromaticColor Green Dark, Chromatic $ ChromaticColor Cyan Dark, Chromatic $ ChromaticColor Blue Dark, Chromatic $ ChromaticColor Magenta Dark]
  , [Black, Chromatic $ ChromaticColor Red Light, White, White, White, White]
  , [Chromatic $ ChromaticColor Red Normal, Chromatic $ ChromaticColor Red Dark, Chromatic $ ChromaticColor Green Dark, Black, White, Chromatic $ ChromaticColor Blue Light]
  , [Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Cyan Normal, Chromatic $ ChromaticColor Magenta Normal, Chromatic $ ChromaticColor Magenta Normal, Chromatic $ ChromaticColor Yellow Normal, Chromatic $ ChromaticColor Cyan Normal]
  , [White, White, White, White, White, White]
  ]

complexCodels ∷ Matrix Color
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
