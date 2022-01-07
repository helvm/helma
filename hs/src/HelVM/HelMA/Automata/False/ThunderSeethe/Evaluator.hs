module HelVM.HelMA.Automata.False.ThunderSeethe.Evaluator where

import           HelVM.HelMA.Automata.False.Util.Util
import           HelVM.HelMA.Automata.False.Value



import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.HighControlOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.MemoryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator
import           HelVM.HelMA.Automaton.Operator.UnaryOperator
import           HelVM.HelMA.Automaton.Operator.Util

import           HelVM.HelMA.Automaton.BIntegral

import qualified Relude.Unsafe                                      as Unsafe

exec :: (BIO m , BIntegral i) => [Value i] -> [Value i] -> [Value i] -> m ([Value i], [Value i])
exec [] stack reg = pure (stack, reg)

exec (Binary operator : vs) (IntLit a : IntLit b : stack) reg = exec vs (IntLit (doBinary operator b a) : stack) reg
exec (Unary  operator : vs) (IntLit a : stack) reg = exec vs (IntLit (doUnary operator a) : stack) reg
exec (Stack operator  : vs) stack reg = exec vs (doStack operator stack) reg

-- Handle Control Flow
exec (High Apply : vs) (Lambda lambda : stack) reg = do
  (newStack, newReg) <- exec lambda stack reg
  exec vs newStack newReg

exec (High When : vs) (fn:IntLit a : stack) reg =
  if integralToBool a
  then exec (High Apply : vs) (fn : stack) reg
  else exec vs stack reg

exec true@(High While : vs) (Lambda body : Lambda cond : stack) reg = do
  (condStack, condReg) <- exec cond stack reg
  if while condStack
  then do
    (newStack, newReg) <- exec body stack condReg
    exec true (Lambda body : Lambda cond : newStack) newReg
  else exec vs stack condReg

-- Handle Standard Input/Output
exec (IOStack InputChar : vs) stack reg = do
  symbol <- wGetCharAs
  exec vs (IntLit symbol : stack) reg
exec (IOStack OutputChar : vs) (IntLit a : stack) reg = do
  wPutAsChar a
  exec vs stack reg
exec (IOStack OutputDec : vs) (IntLit a : stack) reg = do
  wPutStr $ show a
  exec vs stack reg
exec (Print str : vs) stack reg = do
  wPutStr $ toText str
  exec vs stack reg

-- Handle Variables Operations
exec (Memory Store : vs) (Var key : val : stack) reg = exec vs stack (putByIndex reg key val)
exec (Memory Load  : vs) (Var key :       stack) reg = exec vs (getByIndex reg key : stack) reg

-- Handle Values
--exec (Ope op : vs) stack reg = error $ "Invalid operator parameters: " <> show op <> ", nprog " <> show vs <> ", stack " <> show stack <> ", reg" <> show reg --(exec vs stack reg)
--exec ((Ope _) : _) stack reg = pure $ (stack , reg)
exec (CharLit c : vs)   stack reg = exec vs (IntLit (fromIntegral $ charToInteger c) : stack) reg
exec (a : vs)        stack reg = exec vs (a : stack) reg

-- FALSE operators

while :: BIntegral i => [Value i] -> Bool
while = valueToBool . Unsafe.head

putByIndex' :: BIntegral i => [Value i] -> [Value i] -> Value i -> Int -> [Value i]
putByIndex' left (  _ : right) val   0 = left <> [val] <> right
putByIndex' left (mov : right) val key = putByIndex' (left<>[mov]) right val (key-1)
putByIndex'    _            []   _   _ = error "Empty"

putByIndex :: BIntegral i => [Value i] -> Int -> Value i -> [Value i]
putByIndex reg key val = putByIndex' [] reg val key

getByIndex :: BIntegral i => [Value i] -> Int -> Value i
getByIndex (val :   _)   0 = val
getByIndex (  _ : reg) key = getByIndex reg (key - 1)
getByIndex []            _ = error "Empty"
