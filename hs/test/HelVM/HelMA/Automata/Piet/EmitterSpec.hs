module HelVM.HelMA.Automata.Piet.EmitterSpec
  ( spec
  ) where

import           HelVM.HelMA.Automata.Piet.Evaluator
import           HelVM.HelMA.Automata.Piet.FileExtra

import           HelVM.HelMA.Automata.Piet.API.ImageConfig

import           HelVM.HelIO.CartesianProduct
import           HelVM.HelIO.Control.Safe

import           HelVM.GoldenExpectations

import           Codec.Picture                             ( DynamicImage )

import           System.FilePath.Posix

import           Test.Hspec                                ( Spec, describe, it )

spec ∷ Spec
spec =
  describe "Piet Interpreter Golden Tests" $ forM_ ((
    [ "99bottles"
    , "artsy_hello_world"
    , "fizzbuzz"
    , "pi_big"
    , "piet_hello_world"
    , "valentines"
    ] >*< ["pint"]
    ) <> (
    [ "99bottles"
    , "cowsay"
    , "euclid_clint"
    , "fizzbuzz"
    , "GameOfLife"
    , "hello_world_c1"
    , "hello_world_c4"
    , "hello_world_globe"
    , "helloworld-mondrian"
    , "hw5"
    , "japh"
    , "piet_pi"
    , "power2"
    , "primetest2"
    ] >*< ["rpiet"]
    ) <> (
    [ "99bottles"
    , "adder"
    , "alpha_filled"
    , "erat2"
    , "euclid_clint"
    , "euclid_clint_big"
    , "fizzbuzz"
    , "hi"
    , "hw"
    , "hw1-1"
    , "hw5"
    , "hw5_big"
    , "hw_large"
    , "ILoveYouLaura"
    , "japh_big"
    , "piet_factorial"
    , "piet_pi"
    , "piet_pi_big"
    , "pietquest"
    , "power2"
    , "primetest"
    , "primetest2"
    , "test2"
    , "test2_upscaled"
    , "test3"
    , "test_break"
    , "test_push"
    ] >*< ["pietcc"]
    )) $ \(fileName , dirName ) -> do
    let filePath = dirName </> fileName <.> "png"
    let fullPath = "examples" </> "piet" </> filePath
    let path = dirName </> fileName
    describe path $ do
      it ("il" </> path) $
        il fullPath `goldenShouldIO` buildAbsolutePietIlFileName path
      it ("tl" </> path) $
        asm fullPath `goldenShouldIO` buildAbsolutePietTlFileName path
      it ("dot" </> path) $
        dot fullPath `goldenShouldIO` buildAbsolutePietDotFileName path

il ∷ FilePath → IO Text
il path = toText <$> ilL path

ilL ∷ FilePath → IO LText
ilL = emitILIO <=< readImage

emitILIO ∷ DynamicImage → IO LText
emitILIO = safeToIO . emitIL . (defaultConfig, )

asm ∷ FilePath → IO Text
asm path = toText <$> asmL path

asmL ∷ FilePath → IO LText
asmL = asmTextIO <=< readImage

asmTextIO ∷ DynamicImage → IO LText
asmTextIO = safeToIO . emitCommands . (defaultConfig, )

dot ∷ FilePath → IO Text
dot path = toText <$> dotL path

dotL ∷ FilePath → IO LText
dotL = emitDotIO <=< readImage

emitDotIO ∷ DynamicImage → IO LText
emitDotIO = safeToIO . emitDot . (defaultConfig, )
