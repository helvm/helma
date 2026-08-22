
module HelVM.HelMA.Automata.Piet.Free.Piet
  ( Piet
  , PietT
  , runPiet
  ) where

import           HelVM.HelMA.Automata.Piet.Free.ProgramError

import           HelVM.HelMA.Automata.Piet.Types.ProgramConfig
import           HelVM.HelMA.Automata.Piet.Types.ProgramState

runPiet ∷ ProgramConfig → ProgramState → PietT m a → m (Either ProgramError a, ProgramState)
runPiet conf s action = runStateT (runReaderT (runExceptT action) conf) s

type Piet a = PietT IO a

type PietT m a = ExceptT ProgramError (ReaderT ProgramConfig (StateT ProgramState m)) a
