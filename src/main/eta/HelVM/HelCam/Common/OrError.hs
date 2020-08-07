module HelVM.HelCam.Common.OrError where

import Text.Read

genericIndexOrError :: (Show m, Show a, Show i, Integral i) => m ->  [a] -> i -> a
genericIndexOrError message list index = list `genericIndexOrError'` index where
  genericIndexOrError' (x:_)  0 = x
  genericIndexOrError' (_:xs) n
   | 0 < n     = xs `genericIndexOrError'` (n-1)
   | otherwise = error "List.genericIndex: negative argument."
  genericIndexOrError' _ _      = error $ "genericIndexOrError: index too large. " ++ show index ++ " " ++  show list ++ " " ++ show message

readOrError :: Read a => String -> a
readOrError raw = check $ readEither raw where
  check (Right result) = result
  check (Left message) = error $ message ++ " [" ++ raw ++ "]"
