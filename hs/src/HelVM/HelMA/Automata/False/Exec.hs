module HelVM.HelMA.Automata.False.Exec where

import           HelVM.HelMA.Automata.False.Util

import           HelVM.HelMA.Automaton.IO.BusinessIO

import qualified Relude.Unsafe                       as Unsafe

exec :: BIO m => [Value] -> [Value] -> [Value] -> m ([Value], [Value])
exec [] stack reg = return (stack, reg)

-- Handle Arithmetic
exec ((Ope Plus    ):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b+a)):stack) reg
exec ((Ope Minus   ):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b-a)):stack) reg
exec ((Ope Multiply):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b*a)):stack) reg
exec ((Ope Division):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b `div` a)):stack) reg
exec ((Ope Negate  ):prog) ((Num a):stack) reg = exec prog ((Num (negate a)):stack) reg

-- Handle Comparison
exec ((Ope Equal):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (boolToNum (b == a))):stack) reg
exec ((Ope Greater):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (boolToNum (b > a))):stack) reg

-- Handle Boolean Algebra
exec ((Ope And):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b `nAnd` a)):stack) reg
exec ((Ope Or ):prog) ((Num a):(Num b):stack) reg = exec prog ((Num (b `nOr` a)):stack) reg
exec ((Ope Not):prog) ((Num a):stack) reg = exec prog ((Num (nNot a)):stack) reg

-- Handle Stack Operation
exec ((Ope Dup ):prog) (a : stack) reg = exec prog (a:a:stack) reg
exec ((Ope Drop):prog) (_ : stack) reg = exec prog stack reg
exec ((Ope Swap):prog) (a : b : stack) reg = exec prog (b:a:stack) reg
exec ((Ope Rot ):prog) (a : b : c : stack) reg = exec prog (c:a:b:stack) reg
exec ((Ope Pick):prog) ((Num a) : stack ) reg = exec prog ((pick a stack) : stack) reg

-- Handle Control Flow
exec ((Ope Execute):prog) ((Fn lambda):stack) reg = do
  (newStack, newReg) <- exec lambda stack reg
  exec prog newStack newReg

exec ((Ope If):prog) (fn:(Num a):stack) reg =
  if (numToBool a)
  then exec ((Ope Execute):prog) (fn:stack) reg
  else exec prog stack reg

exec true@((Ope While):prog) ((Fn body):(Fn cond):stack) reg = do
  (condStack, condReg) <- exec cond stack reg
  if (while condStack)
  then do
    (newStack, newReg) <- (exec body stack condReg)
    exec true ((Fn body):(Fn cond):newStack) newReg
  else exec prog stack condReg

-- Handle Standard Input/Output
exec ((Io ReadChar):prog) stack reg = do
  char <- wGetChar
  exec prog ((Num (charToNum char)):stack) reg
exec ((Io WriteChar):prog) ((Num a):stack) reg = do
  wPutChar $ numToChar a
  exec prog stack reg
exec ((Io WriteDec):prog) ((Num a):stack) reg = do
  wPutStr $ show a
  exec prog stack reg
exec ((Io (Str str)):prog) stack reg = do
  wPutStr $ toText str
  exec prog stack reg

-- Handle Variables Operations
exec ((Ope Store):prog) ((Var key):val:stack) reg = exec prog stack (putByIndex reg key val)
exec ((Ope Fetch):prog) ((Var key):stack) reg = exec prog ((getByIndex reg key):stack) reg

-- Handle Values
exec ((Ope op):prog) stack reg = error ("Invalid operator parameters: " <> (show op) <> ", nprog " <> (show prog) <> ", stack " <> (show stack) <> ", reg" <> (show reg)) --(exec prog stack reg)
--exec ((Ope _):_) stack reg = pure $ (stack , reg)
exec ((Ch c):prog)   stack reg = exec prog ((Num (charToNum c)):stack) reg
exec (a:prog)        stack reg = exec prog (a:stack) reg

-- FALSE operators
pick :: Integer -> [Value] -> Value
pick n stack = Unsafe.head $ drop (fromInteger n) stack

while :: [Value] -> Bool
while = valueToBool . Unsafe.head

putByIndex' :: [Value] -> [Value] -> Value -> Int -> [Value]
putByIndex' left (  _ : right) val   0 = left <> [val] <> right
putByIndex' left (mov : right) val key = putByIndex' (left<>[mov]) right val (key-1)
putByIndex'    _            []   _   _ = error "Empty"


putByIndex :: [Value] -> Int -> Value -> [Value]
putByIndex reg key val = putByIndex' [] reg val key

getByIndex :: [Value] -> Int -> Value
getByIndex (val :   _)   0 = val
getByIndex (  _ : reg) key = getByIndex reg (key - 1)
getByIndex []            _ = error "Empty"

-- Boolean Num Operators
nAnd :: Integer -> Integer -> Integer
nAnd 0 0     = 0
nAnd   _ 0   = 0
nAnd 0   _   = 0
nAnd   _   _ = 1

nNot :: Integer -> Integer
nNot 0   = 1
nNot   _ = 0

nOr :: Integer -> Integer -> Integer
nOr 0 0     = 0
nOr   _ 0   = 1
nOr 0   _   = 1
nOr   _   _ = 1
