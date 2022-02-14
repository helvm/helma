module HelVM.HelMA.Automata.Zot.Evaluator (
  eval,
) where

import           HelVM.HelMA.Automaton.API.IOTypes

import           HelVM.Common.Containers.Util
import           HelVM.Common.Control.Safe

import           HelVM.Common.Digit.Digitable
import           HelVM.Common.Digit.ToDigit

import           HelVM.Common.ListLikeUtil

import           Control.Monad.Writer.Lazy


import qualified Data.DList                        as D
import qualified Data.Text                         as Text

import           Text.Read
import qualified Text.Show

eval :: MonadSafe m => Bool -> Source -> Input -> m Output
eval False source input = pure $ showFoldable $ internalRun2 source input
eval True  source input = makeAsciiText28 . convert . internalRun2 source =<< asciiTextToZotText input

asciiTextToZotText :: MonadSafe m => Input -> m Output
asciiTextToZotText input = showFunList <$> textToDL input

internalRun2 :: Source -> Input -> FunDList
internalRun2 source input = internalRun $ source <> input

internalRun :: Text -> FunDList
internalRun = execWriter . interpret

interpret :: Text -> Out ()
interpret source = readZot source >><< outputFun >>< printFun *> tell D.empty

readZot :: Text -> Out Fun
readZot = foldFun . readTextAsFunList

foldFun :: FunList -> Out Fun
foldFun = foldM (><) emptyFun

outputFun :: Out Fun
outputFun = kFun ><< kFun ><< kFun ><< kFun ><< kFun ><< kFun >< iFun

printFun :: Fun
printFun = Fun innerFun

innerFun :: Fun -> Out Fun
innerFun f = interrogateFun f >>< Zero >>< One >>= tell . D.singleton >> pure printFun

interrogateFun :: Fun -> Out Fun
interrogateFun f = f >< iFun >>< iFun >>< iFun >>< kFun

infixl 9 ><
(><) :: Fun -> Fun -> Out Fun
(><) Zero    = (zeroFun ><)
(><) One     = (oneFun ><)
(><) (Fun f) = f

infixl 6 >><
(>><) :: Out Fun -> Fun -> Out Fun
f >>< a = f >>= (>< a)

infixr 8 ><<
(><<) :: Fun -> Out Fun -> Out Fun
f ><< a = (f ><) =<< a

infixl 7 >><<
(>><<) :: Out Fun -> Out Fun -> Out Fun
f >><< a = f >>= (><< a)

emptyFun :: Fun
emptyFun = contFun iFun

zeroFun :: Fun
zeroFun = contFun $ Fun $ \f -> f >< sFun >>< kFun

oneFun :: Fun
oneFun = makeFun $ \c -> contFun $ makeFun $ \l -> contFun $ Fun $ \r -> c ><< l >< r

contFun :: Fun -> Fun
contFun = Fun . flip (><)

sFun :: Fun
sFun = makeFun $ \x -> makeFun $ \y -> Fun $ \z -> x >< z >><< y >< z

kFun :: Fun
kFun = makeFun $ makeFun . const

iFun :: Fun
iFun = makeFun id

makeFun :: (Fun -> Fun) -> Fun
makeFun f =  Fun $ pure . f

showFunList :: FunList -> Text
showFunList = showFoldable

readTextAsFunList :: Text -> FunList
readTextAsFunList = concatMap readTextLineAsFunList . lines

readTextLineAsFunList :: Text -> FunList
readTextLineAsFunList = textToFunList . filter01 . Text.takeWhile (/= '#')

filter01 ::Text -> Text
filter01 = Text.filter is01

is01 :: Char -> Bool
is01 c = c == '0' || c == '1'

textToFunList :: Text -> FunList
textToFunList = stringToFunList . toString

stringToFunList :: String -> FunList
stringToFunList s = charToFunList =<< s

charToFunList :: Char -> FunList
charToFunList = maybeToList . rightToMaybe . charToFunSafe

charToFun :: Char -> Fun
charToFun = unsafe . charToFunSafe

charToFunSafe :: MonadSafe m => Char -> m Fun
charToFunSafe '0' = pure Zero
charToFunSafe '1' = pure One
charToFunSafe  c  = liftErrorWithPrefix "charToFun" $ one c

-- | Types
type FunDList = D.DList Fun

type FunList = [Fun]

data Fun = Zero | One | Fun (Fun -> Out Fun)

type Out = Writer FunDList

instance Read Fun where
  readsPrec _ []      = []
  readsPrec _ (c : s) = [(charToFun c , s)]
  readList s = [(stringToFunList s , "")]

instance Show Fun where
  show Zero    = "0"
  show One     = "1"
  show (Fun _) = "function"
  showList fs  = (concatMap show fs <>)

instance Digitable Fun where
  fromDigit 0 = pure Zero
  fromDigit 1 = pure One
  fromDigit t = liftErrorWithPrefix "Wrong token" $ show t

instance ToDigit Fun where
  toDigit Zero = pure 0
  toDigit One  = pure 1
  toDigit t    = liftErrorWithPrefix "Wrong token" $ show t
