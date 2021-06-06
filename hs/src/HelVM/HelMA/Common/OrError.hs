module HelVM.HelMA.Common.OrError where

readOrError :: Read r => String -> r
readOrError raw = check $ readEither raw where
  check (Right result) = result
  check (Left message) = error $ message <> " [" <> toText raw <> "]"

infix 9 !!!
(!!!) :: (Show a) => [a] -> Int -> a
(!!!) list index = check $ list !!? index where
  check (Just result) = result
  check  Nothing      = error $ "OnError.!!!" <> show index <> " " <>  show list

indexOrError :: (Show m , Show a) => m -> [a] -> Int -> a
indexOrError message list index = check $ list !!? index where
  check (Just result) = result
  check  Nothing      = error $ "OnError.indexOrError\n" <> show index <> "\n" <> show list <> "\n" <> show message
