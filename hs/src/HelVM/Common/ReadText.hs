module HelVM.Common.ReadText (
  readText,
  readTextSafe,
  readTextMaybe,
) where

import HelVM.Common.Safe

readText :: Read a => Text -> a
readText = unsafe . readTextSafe

readTextSafe :: Read a => Text -> Safe a
readTextSafe a = appendErrorTuple ("" , a) $ readEither $ toString a

readTextMaybe :: Read a => Text -> Maybe a
readTextMaybe = readMaybe . toString
