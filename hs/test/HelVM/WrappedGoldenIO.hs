module HelVM.WrappedGoldenIO where

import System.FilePath.Posix

import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.Core.Spec

(<->) :: FilePath -> FilePath -> FilePath
(<->) major minor = major <> "-" <> minor

infix 1 `goldenShouldReturn`
goldenShouldReturn :: IO Text -> FilePath -> WrappedGoldenIO Text
goldenShouldReturn actualOutputIO fileName = WrappedGoldenIO $ flip goldenShouldBe fileName <$> actualOutputIO

infix 1 `goldenShouldBe`
goldenShouldBe :: Text -> FilePath -> Golden Text
goldenShouldBe actualOutput fileName =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFileText,
    readFromFile = readFileText,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

----

newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }

type GoldenIO a = IO (Golden a)

----

instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
