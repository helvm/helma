module HelVM.HelMA.Automata.Expectations where

import System.FilePath.Posix

import Test.Hspec
import Test.Hspec.Golden
import Test.Hspec.Core.Spec

(<->) :: FilePath -> FilePath -> FilePath
(<->) major minor = major <> "-" <> minor

infix 1 `goldenShouldReturn`
goldenShouldReturn :: IO String -> String -> WrappedGoldenIO String
goldenShouldReturn actualOutputIO fileName = WrappedGoldenIO $ flip goldenShouldBe fileName <$> actualOutputIO

infix 1 `goldenShouldBe`
goldenShouldBe :: String -> String -> Golden String
goldenShouldBe actualOutput fileName =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

infix 1 `ioShouldBe`
ioShouldBe :: (HasCallStack , Show a , Eq a) => IO a -> IO a -> Expectation
ioShouldBe action expected = join $ liftA2 shouldBe action expected

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack , Show a , Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected

----

newtype WrappedGoldenIO a = WrappedGoldenIO { unWrappedGoldenIO :: GoldenIO a }

type GoldenIO a = IO (Golden a)

----

instance Eq str => Example (WrappedGoldenIO str) where
  type Arg (WrappedGoldenIO str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unWrappedGoldenIO wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
