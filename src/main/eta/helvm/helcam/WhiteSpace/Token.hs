module HelVM.HelCam.WhiteSpace.Token where

data Token =  S | T | N 
  deriving (Eq, Ord, Enum)

type TokenList = [Token]

instance Show Token where
  show S = " "
  show T = "\t"
  show N = "\n"

-- Scanner
instance Read Token where
  readsPrec _ " "  = [( S , "")]
  readsPrec _ "\t" = [( T , "")]
  readsPrec _ "\n" = [( N , "")]
  readsPrec _ _    = []

toBit :: (Num a) => Token -> a
toBit S = 0
toBit T = 1
toBit N = error $ show N
