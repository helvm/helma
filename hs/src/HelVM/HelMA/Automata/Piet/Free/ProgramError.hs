module HelVM.HelMA.Automata.Piet.Free.ProgramError
  ( ProgramError (..)
  ) where

import qualified RIO

data ProgramError
  = ParseInt Text
  | LoadFile Text
  | FindFile Text
  | NotImplemented Text
  deriving stock (Eq, Show)

instance RIO.Display ProgramError where
  textDisplay (ParseInt m)       = "Error while parsing: " <> m
  textDisplay (LoadFile m)       = "Error while loading file: " <> m
  textDisplay (FindFile m)       = "Can't find file: " <> m
  textDisplay (NotImplemented m) = m <> " hasn't been implemented yet."
