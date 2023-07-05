module HelVM.HelMA.Automata.Piet.CodelChooser where

toggle :: Int -> CodelChooser -> CodelChooser
toggle n cc = let n' = (n + fromEnum cc) `rem` 2
  in toEnum $ if n' < 0 then n' + 2 else n'

data CodelChooser = CCLeft | CCRight
  deriving stock (Show, Read, Eq, Ord, Enum)
