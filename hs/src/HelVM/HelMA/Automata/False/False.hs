module HelVM.HelMA.Automata.False.False where

import           HelVM.HelMA.Automata.False.Exec
import           HelVM.HelMA.Automata.False.Parser
import           HelVM.HelMA.Automata.False.Util
import           System.Environment

main = do
	args <- getArgs
	prog <- readFile $ head args
	(stack, reg) <- exec (parse prog) [] emptyReg -- init a-z registers to 0
	putStrLn $ show stack
