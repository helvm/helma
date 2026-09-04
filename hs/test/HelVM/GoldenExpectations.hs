module HelVM.GoldenExpectations
  ( goldenLShouldBe
  , goldenLShouldIO
  , goldenShouldBe
  , goldenShouldBusinessT
  , goldenShouldIO
  , goldenShouldSafe
  , goldenShouldSafeT
  , (<->)
  ) where

import           HelVM.HelIO.Control.Business
import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           Control.Type.Operator
import           System.FilePath.Posix

import           Test.Hspec.Core.Spec
import           Test.Hspec.Golden

infixl 1 <->
(<->) ∷ FilePath → FilePath → FilePath
(<->) major minor = major <> "-" <> minor

infix 1 `goldenShouldBusinessT`
goldenShouldBusinessT ∷ BusinessT IO Text → FilePath → GoldenExpectations Text
goldenShouldBusinessT actualOutput = goldenShouldIO (businessTToIOWithLogs actualOutput)

infix 1 `goldenShouldSafeT`
goldenShouldSafeT ∷ SafeT IO Text → FilePath → GoldenExpectations Text
goldenShouldSafeT actualOutput = goldenShouldIO (safeTToIO actualOutput)

infix 1 `goldenShouldSafe`
goldenShouldSafe ∷ Safe Text → FilePath → GoldenExpectations Text
goldenShouldSafe actualOutputSafe = goldenShouldIO (safeToIO actualOutputSafe)


infix 1 `goldenLShouldIO`
goldenLShouldIO ∷ IO LText → FilePath → GoldenExpectations LText
goldenLShouldIO actualOutputIO fileName = GoldenExpectations $ flip goldenLShouldBe fileName <$> actualOutputIO

infix 1 `goldenShouldIO`
goldenShouldIO ∷ IO Text → FilePath → GoldenExpectations Text
goldenShouldIO actualOutputIO fileName = GoldenExpectations $ flip goldenShouldBe fileName <$> actualOutputIO

infix 1 `goldenLShouldBe`
goldenLShouldBe ∷ LText → FilePath → Golden LText
goldenLShouldBe actualOutput fileName =
  Golden {
    output       = actualOutput,
    encodePretty = show,
    writeToFile  = writeFileLText,
    readFromFile = readFileLTextUtf8,
    goldenFile   = ".output" </> "golden" </> fileName,
    actualFile   = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

-- DEPRECATED
infix 1 `goldenShouldBe`
goldenShouldBe ∷ Text → FilePath → Golden Text
goldenShouldBe actualOutput fileName =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFileText,
    readFromFile = readFileTextUtf8,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

----

newtype GoldenExpectations a
  = GoldenExpectations { unGoldenExpectations :: GoldenIO a }

type GoldenIO a = IO $ Golden a

----

instance Eq str ⇒ Example (GoldenExpectations str) where
  type Arg (GoldenExpectations str) = ()
  evaluateExample wrapped params action callback = build =<< unGoldenExpectations wrapped where
    build unwrapped = evaluateExample unwrapped params action callback
