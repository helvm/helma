module HelVM.HelMA.Automata.False.Util where

data Operator =
    Dup
  | Drop
  | Swap
  | Rot
  | Pick
  | Plus
  | Minus
  | Multiply
  | Division
  | Negate
  | And
  | Or
  | Not
  | Greater
  | Equal
  | Comment
  | Execute
  | If
  | While
  | Store
  | Fetch
  deriving stock (Eq , Show , Read)

data StdIO =
    ReadChar
  | WriteChar
  | WriteDec
  | Str String
  | Flush
  deriving stock (Eq , Show , Read)

data Value =
    Ope Operator
  | Num Integer
  | Ch Char
  | Io StdIO
  | Fn [Value]
  | Var Int
  deriving stock (Eq , Show , Read)

-- Util Functions
emptyReg :: [Value]
emptyReg = take 26 $ repeat (Num 0)

-- Converters
valueToBool :: Value -> Bool
valueToBool (Num 0) = False
valueToBool (Num _) = True
valueToBool _       = False

charToVar :: Char -> Int
charToVar c = (ord c) - (ord 'a')

charToNum :: Char -> Integer
charToNum = fromIntegral . ord

numToChar :: Integer -> Char
numToChar = chr . fromInteger

boolToNum :: Bool -> Integer
boolToNum True  = 1
boolToNum False = 0

numToBool :: Integer -> Bool
numToBool 0 = False
numToBool _ = True
