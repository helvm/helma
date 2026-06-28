module HelVM.HelMA.Automaton.Types.FileType where

import           Data.Default

defaultFileFormat :: FileType
defaultFileFormat = def

fileFormats :: NonEmpty FileType
fileFormats = universeNonEmpty

data FileType = TextFile | BinaryFile
  deriving stock (Bounded , Enum , Eq , Read , Show)

instance Default FileType where
  def = minBound
