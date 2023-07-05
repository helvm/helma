module HelVM.HelMA.Automata.Piet.MonadFailExtra where

eitherToMonadFail :: (Show a, MonadFail m) => Either a b -> m b
eitherToMonadFail (Right a) = pure a
eitherToMonadFail (Left  a) = fail $ show a
