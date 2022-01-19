module HelVM.GoldenExpectations (
  (<->),

  goldenShouldControlT,
  goldenShouldSafeT,
  goldenShouldSafe,
  goldenShouldIO,
  goldenShouldBe,
) where

import           HelVM.Common.Control.Control
import           HelVM.Common.Control.Safe

import           Control.Type.Operator
import           System.FilePath.Posix

import           Test.Hspec.Core.Spec
import           Test.Hspec.Golden

infixl 1 <->
(<->) :: FilePath -> FilePath -> FilePath
(<->) major minor = major <> "-" <> minor

infix 1 `goldenShouldControlT`
goldenShouldControlT :: ControlT IO Text -> FilePath -> GoldenExpectations Text
goldenShouldControlT actualOutput = goldenShouldIO (controlTToIOWithLogs actualOutput)

infix 1 `goldenShouldSafeT`
goldenShouldSafeT :: SafeT IO Text -> FilePath -> GoldenExpectations Text
goldenShouldSafeT actualOutput = goldenShouldIO (safeTToIO actualOutput)

infix 1 `goldenShouldSafe`
goldenShouldSafe :: Safe Text -> FilePath -> GoldenExpectations Text
goldenShouldSafe actualOutputSafe = goldenShouldIO (safeToIO actualOutputSafe)

infix 1 `goldenShouldIO`
goldenShouldIO :: IO Text -> FilePath -> GoldenExpectations Text
goldenShouldIO actualOutputIO fileName = GoldenExpectations $ flip goldenShouldBe fileName <$> actualOutputIO

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

newtype GoldenExpectations a = GoldenExpectations { unGoldenExpectations :: GoldenIO a }

type GoldenIO a = IO $ Golden a

----

instance Eq str => Example (GoldenExpectations str) where
  type Arg (GoldenExpectations str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unGoldenExpectations wrapped where
    evaluateExample' golden = evaluateExample golden params action callback
