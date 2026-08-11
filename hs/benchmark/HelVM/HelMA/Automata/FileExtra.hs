module HelVM.HelMA.Automata.FileExtra
    ( options
    , readSourceFile
    ) where

import           HelVM.HelIO.Extra

import           System.FilePath.Posix

readSourceFile ∷ MonadIO m ⇒ FilePath → m Text
readSourceFile filePath = readFileTextUtf8 $ "examples" </> filePath

options ∷ [Bool]
options = [False , True]
