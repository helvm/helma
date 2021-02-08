module HelVM.HelCam.Common.OrError where

readOrError :: Read r => String -> r
readOrError raw = check $ readEither raw where
  check (Right result) = result
  check (Left message) = error $ message <> " [" <> toText raw <> "]"

infix 9 !!!
(!!!) :: (Show a) => [a] -> Int -> a
(!!!) list index = check $ list !!? index where
  check (Just result) = result
  check  Nothing      = error $ "OnError.!!!" <> show index <> " " <>  show list

indexOrError :: (Show m, Show a) => m -> [a] -> Int -> a
indexOrError message list index = check $ list !!? index where
  check (Just result) = result
  check  Nothing      = error $ "OnError.indexOrError" <> show index <> " " <>  show list <> " " <> show message

genericIndexOrError :: (Show m, Show a, Show i, Integral i) => m -> [a] -> i -> a
genericIndexOrError message list index = recur list index where
  recur (x:_)  0 = x
  recur (_:xs) i
   | 0 < i       = recur xs $ i-1
   | otherwise   = error $ "OrError.genericIndexOrError: negative index." <> show index <> " " <>  show list <> " " <> show message
  recur _ _      = error $ "OrError.genericIndexOrError: too large index." <> show index <> " " <>  show list <> " " <> show message
