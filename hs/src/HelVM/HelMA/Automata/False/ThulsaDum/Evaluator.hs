module HelVM.HelMA.Automata.False.ThulsaDum.Evaluator where

import           HelVM.HelMA.Automata.False.Util.Util
import           HelVM.HelMA.Automata.False.Value

import           HelVM.HelMA.Automaton.IO.BusinessIO

import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.HighControlOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator
import           HelVM.HelMA.Automaton.Operator.UnaryOperator

import           HelVM.HelMA.Automaton.BIntegral

import           Control.Monad.Except                               hiding (runExceptT)
import           Control.Monad.RWS.Strict                           hiding (pass)

import qualified Data.Map.Strict                                    as Map


runEval :: (BusinessIO m , BIntegral i) => [Value i] -> m (Either String [i])
runEval = runFalseInterpreter' . eval

runEval_ :: (BusinessIO m , BIntegral i) => [Value i] -> m (Either String ())
runEval_ = runFalseInterpreter' . eval_

eval :: (BusinessIO m , BIntegral i) => [Value i] -> FalseInterpreter m i [i]
eval ts = do
  mapM_ evalStep ts
  FIState {stack = stack} <- get
  pure stack

eval_ :: (BusinessIO m , BIntegral i) => [Value i] -> FalseInterpreter m i ()
eval_ ts = eval ts $> ()

runFalseInterpreter' :: (BusinessIO m , Integral i) => FalseInterpreter m i a -> m (Either String a)
runFalseInterpreter' interp = runFalseInterpreter interp defaultEnv emptyState


runFalseInterpreter :: BusinessIO m => FalseInterpreter m i a -> FIEnv -> FIState i -> m (Either String a)
runFalseInterpreter interp env st = runExceptT $ extract <$> (runRWST . unwrap) interp env st where extract (a , _ , _) = a

-- TODO improve error handling
evalStep :: (BusinessIO m,  BIntegral i) => Value i -> FalseInterpreter m i ()

evalStep (Binary operator) = evalBinOp $ doBinary operator

evalStep (Unary operator) = evalOp $ doUnary operator

evalStep (IntLit n) = push n
evalStep (CharLit ch) = push . fromIntegral . ord $ ch

evalStep (PutVar v) = pop >>= putVar v
evalStep (GetVar v) = getVar v >>= push
evalStep (Memory _) = error "evalStep"

evalStep (Lambda ts) = addLambda ts
evalStep (High Apply) = pop >>= getLambda >>= eval_

evalStep (Stack Dup) = top >>= push
evalStep (Stack Discard) = pop $> ()
evalStep (Stack Swap) = do
  val1 <- pop
  val2 <- pop
  push val1
  push val2
evalStep (Stack Rot) = do
  val1 <- pop
  val2 <- pop
  val3 <- pop
  push val2
  push val1
  push val3
evalStep (Stack DCopy) = do
  n <- pop
  val <- peek . fromIntegral $ n
  push val

evalStep (Stack _) = error "evalStep"

evalStep (High When) = do
  l <- pop
  cond <- pop
  if cond == 0
    then pass
    else getLambda l >>= eval_
evalStep (High While) = do
  lbody <- pop
  lcond <- pop
  getLambda lcond >>= eval_
  cond <- pop
  if cond == 0
    then pass
    else do
    getLambda lbody >>= eval_
    push lcond *> push lbody
    evalStep (High While)

evalStep (Print msg) = aaa $ wPutStr $ toText msg

evalStep (IOStack OutputDec) = pop >>= (aaa . wPutStr . show)
evalStep (IOStack OutputChar)    = pop >>= (aaa . wPutChar . chr . fromIntegral)
evalStep (IOStack InputChar)     = ddd wGetChar >>= (push . fromIntegral . ord)
evalStep (IOStack _)             = error "evalStep"
evalStep Flush                   = aaa wFlush
evalStep (Var i)                 = error $ "evalStep Var " <> show i

aaa :: Monad m => m a -> FalseInterpreter m i ()
aaa f = FalseInterpreter {unwrap = mapRWST (mapExceptT (<* f)) (unwrap pass)}

bbb :: Monad m => m (Either String (a, FIState i, FIWriter)) -> FalseInterpreter m i a
bbb f = FalseInterpreter {unwrap = mapRWST (mapExceptT (*> f)) (unwrap pass)}

ddd :: Monad m => m a -> FalseInterpreter m i a
ddd f = FalseInterpreter {unwrap = mapRWST (mapExceptT (ccc f)) (unwrap pass)}

ccc :: (Monad m, Functor f) => m a1 -> m (f (a2, b, c)) -> m (f (a1, b, c))
ccc f b = do
  a <- f
  e <- b
  pure $ (\ (_ , s , w) -> (a , s , w)) <$> e

evalOp :: BusinessIO m => (i -> i) -> FalseInterpreter m i ()
evalOp f = pop >>= push . f

evalBinOp :: BusinessIO m => (i -> i -> i) -> FalseInterpreter m i ()
evalBinOp rel = do
  op1 <- pop
  op2 <- pop
  push $ rel op2 op1


----

interpretingError :: BusinessIO m => String -> FalseInterpreter m i a
interpretingError msg = FalseInterpreter $ throwError msg



pop :: BusinessIO m => FI m i
pop = do
  FIState {stack = stack} <- get
  case stack of
    [] -> interpretingError "stack underflow"
    (x:xs) -> do
      modify $ \s -> s { stack = xs}
      pure x

push :: BusinessIO m => i -> FalseInterpreter m i ()
push x = modify $ \s@FIState {stack = xs} -> s {stack = x : xs}


-- | returns the nth element of the stack
peek :: BusinessIO m => Int -> FI m i
peek n = do
  FIState {stack = stack} <- get
  case subscript stack n of
    Nothing -> interpretingError "stack underflow"
    Just x  -> pure x

top :: BusinessIO m => FI m i
top = peek 0

getVar :: (BusinessIO m , Num i) => Char -> FI m i
getVar var = do
  FIState { variables = vars } <- get
  pure $ Map.findWithDefault 0 var vars

putVar :: BusinessIO m => Char -> i -> FalseInterpreter m i ()
putVar var val =  modify $ \st@FIState {variables = vars} -> st { variables = Map.alter f var vars } where f _ = pure val

addLambda :: (BusinessIO m , Num i, Ord i) => [Value i] -> FalseInterpreter m i ()
addLambda ts = do
  st@FIState { lambdas = ls } <- get
  let id' = fromIntegral . Map.size $ ls
  put $ st { lambdas = Map.insert id' ts ls }
  push id'

getLambda :: (BusinessIO m , Ord i) => i -> FalseInterpreter m i [Value i]
getLambda i = do
  FIState { lambdas = ls } <- get
  pure $ Map.findWithDefault [] i ls

-- | Constructors

emptyState :: (Integral i) => FIState i
emptyState = FIState
  { stack = []
  , variables = Map.empty
  , lambdas = Map.empty
  , currentLambda = 0
  }

defaultEnv :: FIEnv
defaultEnv = ()

-- | Types

type FI m i = FalseInterpreter m i i

newtype FalseInterpreter m i a = FalseInterpreter { unwrap :: RWST FIEnv FIWriter (FIState i) (ExceptT String m) a }
  deriving newtype ( Functor, Applicative, Monad, MonadState (FIState i), MonadReader FIEnv, MonadWriter FIWriter, MonadIO)

type FIEnv = ()
type FIWriter = ()

data FIState i = FIState
  { stack         :: [i]
  , variables     :: Map Char i
  , lambdas       :: Map i [Value i]
  , currentLambda :: i
  }
