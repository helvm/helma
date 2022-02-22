module HelVM.HelMA.Automata.Zot.Evaluator (
  evalExpressionList,
)  where

import           HelVM.HelMA.Automata.Zot.Expression

import           Control.Monad.Writer.Lazy

import qualified Data.ListLike                       as LL

-- | High-level Expressions
evalExpressionList :: ExpressionList -> Out Expression
evalExpressionList el = foldExpression el >><< outputExpression >>< printExpression

foldExpression :: ExpressionList -> Out Expression
foldExpression = foldM (><) emptyExpression

emptyExpression :: Expression
emptyExpression = contExpression iExpression

outputExpression :: Out Expression
outputExpression = kExpression ><< kExpression ><< kExpression ><< kExpression ><< kExpression ><< kExpression >< iExpression

printExpression :: Expression
printExpression = Expression innerPrintExpression

innerPrintExpression :: Expression -> Out Expression
innerPrintExpression f = interrogateExpression f >>< Zero >>< One >>= tell . LL.singleton >> pure printExpression

interrogateExpression :: Expression -> Out Expression
interrogateExpression f = f >< iExpression >>< iExpression >>< iExpression >>< kExpression

-- | Operators
infixl 9 ><
(><) :: Expression -> Expression -> Out Expression
(><) Zero           = (zeroExpression ><)
(><) One            = (oneExpression ><)
(><) (Expression f) = f

infixl 6 >><
(>><) :: Out Expression -> Expression -> Out Expression
f >>< a = f >>= (>< a)

infixr 8 ><<
(><<) :: Expression -> Out Expression -> Out Expression
f ><< a = (f ><) =<< a

infixl 7 >><<
(>><<) :: Out Expression -> Out Expression -> Out Expression
f >><< a = f >>= (><< a)


-- | Low-level Expressions
zeroExpression :: Expression
zeroExpression = contExpression $ Expression $ \ f -> f >< sExpression >>< kExpression

oneExpression :: Expression
oneExpression = makeExpression $ \c -> contExpression $ makeExpression $ \l -> contExpression $ Expression $ \r -> c ><< l >< r

contExpression :: Expression -> Expression
contExpression = Expression . flip (><)

sExpression :: Expression
sExpression = makeExpression $ \x -> makeExpression $ \y -> Expression $ \z -> x >< z >><< y >< z

kExpression :: Expression
kExpression = makeExpression $ makeExpression . const

iExpression :: Expression
iExpression = makeExpression id

makeExpression :: (Expression -> Expression) -> Expression
makeExpression f =  Expression $ pure . f
