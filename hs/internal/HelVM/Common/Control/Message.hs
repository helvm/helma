module HelVM.Common.Control.Message where

import qualified Data.DList as D

-- | Destructors
errorsToText :: Messages -> Text
errorsToText = unlines . D.toList

errorsToString :: Messages -> String
errorsToString = toString . errorsToText

-- | Constructors
stringToErrors :: String -> Messages
stringToErrors = D.singleton . toText

tupleListToMessage :: [MessageTuple] -> Message
tupleListToMessage xs = mconcat $ tupleToMessage <$> xs

tupleToMessage :: MessageTuple -> Message
tupleToMessage (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

-- | Types
type MessageTuple = (Message , Message)

type Messages = D.DList Text

type Message = Text
