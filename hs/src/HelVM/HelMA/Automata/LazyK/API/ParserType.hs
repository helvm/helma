module HelVM.HelMA.Automata.LazyK.API.ParserType where

parseParserType :: String -> ParserType
parseParserType raw = valid $ readMaybe raw where
  valid (Just value) = value
  valid Nothing      = error $ "'" <> toText raw <> "' is not valid ParserType. Valid parserTypes are : " <> show parserTypes

defaultParserType :: ParserType
defaultParserType = Mixed

parserTypes :: [ParserType]
parserTypes = [Mixed , UnLambda , Combinator]

homogeneousParserTypes :: [ParserType]
homogeneousParserTypes = [UnLambda , Combinator]

data ParserType = Mixed | UnLambda | Combinator | Jot | Iota
  deriving stock (Bounded , Enum , Eq , Read , Show)
