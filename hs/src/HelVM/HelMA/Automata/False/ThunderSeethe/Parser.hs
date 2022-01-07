module HelVM.HelMA.Automata.False.ThunderSeethe.Parser where

import           HelVM.HelMA.Automata.False.Util.Util
import           HelVM.HelMA.Automata.False.Value

import           HelVM.HelMA.Automaton.Operator.BinaryOperator
import           HelVM.HelMA.Automaton.Operator.HighControlOperator
import           HelVM.HelMA.Automaton.Operator.IOOperator
import           HelVM.HelMA.Automaton.Operator.MemoryOperator
import           HelVM.HelMA.Automaton.Operator.StackOperator
import           HelVM.HelMA.Automaton.Operator.UnaryOperator

import           HelVM.HelMA.Automaton.BIntegral

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ReadText

import           Data.Char                                          hiding (chr)


parseFromTextSafe :: BIntegral i => Text -> Safe [Value i]
parseFromTextSafe = pure . parse . toString

parse :: BIntegral i => String -> [Value i]
parse [] = []
-- Parse Arithmetic
parse ('+' : source) = Binary Add : parse source
parse ('-' : source) = Binary Sub : parse source
parse ('*' : source) = Binary Mul : parse source
parse ('/' : source) = Binary Div : parse source
parse ('_' : source) = Unary  Neg : parse source

-- Parse Comparison
parse ('=' : source) = Binary (blEQ bl) : parse source
parse ('>' : source) = Binary (blGT bl)  : parse source

-- Parse Boolean Algebra
parse ('&' : source) = Binary (blAnd bl)  : parse source
parse ('|' : source) = Binary (blOr bl)  : parse source
parse ('~' : source) = Unary (blNot bl)  : parse source

-- Parse Stack Ope
parse ('$' : source)  = Stack Dup : parse source
parse ('%' : source)  = Stack Discard : parse source
parse ('\\' : source) = Stack Swap : parse source
parse ('@' : source)  = Stack Rot : parse source
parse ('o' : source)  = Stack DCopy : parse source

-- Parse Control Flow
parse ('!' : source) = High Apply : parse source
parse ('?' : source) = High When : parse source
parse ('#' : source) = High While : parse source
parse ('{' : source) = parse (afterComment source)
parse ('[' : source) = Lambda (parse $ parseLambda 0 source) : parse (afterLambda 0 source)

-- Parse Standard Input/Output
parse ('^' : source) = IOStack InputChar : parse source
parse (',' : source) = IOStack OutputChar : parse source
parse ('.' : source) = IOStack OutputDec : parse source
parse ('"' : source) = Print (parseString source) : parse (afterString source)

-- Parse Variables
parse (c : source) | isAlpha c = Var (charToVar c) : parse source
parse (':' : source) = Memory Store : parse source
parse (';' : source) = Memory Load  : parse source

-- Parse Literals
parse ('\'' : c : source) = CharLit c : parse source
parse full@(i : source) | isDigit i = IntLit (readTextUnsafe $ toText $ parseNum full) : parse (afterNum source)

-- Skip other shit
parse (_ : source) = parse source


-- Parse manipulators
-- Parse Strings
parseString :: String -> String
parseString ('"' : _)             = []
-- Handle special characters explicitly
parseString ('\\' : 'a' : source) = chr 7 : parseString source
parseString ('\\' : 'b' : source) = chr 8 : parseString source
parseString ('\\' : 'f' : source) = chr 12 : parseString source
parseString ('\\' : 'n' : source) = chr 10 : parseString source
parseString ('\\' : 'r' : source) = chr 13 : parseString source
parseString ('\\' : 't' : source) = chr 9 : parseString source
parseString ('\\' : '"' : source) = chr 34 : parseString source
parseString (c : source)          = c:parseString source
parseString []                    = error "Empty"

afterString :: String -> String
afterString ('"' : source) = source
afterString (_   : source) = afterString source
afterString []             = error "Empty"


-- Parse Lambdas
parseLambda :: Int -> String -> String
parseLambda _ []             = error "Unclosed lambda"
parseLambda 0 (']' : _)      = []
parseLambda n (']' : source) = ']' : parseLambda (n-1) source
parseLambda n ('[' : source) = '[' : parseLambda (n+1) source
parseLambda n ( c  : source) =  c  : parseLambda n source

afterLambda :: Int -> String -> String
afterLambda _ []             = error "Unclosed lambda"
afterLambda 0 (']' : source) = source
afterLambda n (']' : source) = afterLambda (n-1) source
afterLambda n ('[' : source) = afterLambda (n+1) source
afterLambda n (_   : source) = afterLambda n source

-- Parse Numbers
parseNum :: String -> String
parseNum [] = []
parseNum ('.' : i : source) | isDigit i = '.' : i : parseNum source
parseNum       (i : source) | isDigit i =       i : parseNum source
parseNum (_ : _) = []

afterNum :: String -> String
afterNum [] = []
afterNum (i : source) | isDigit i = afterNum source
afterNum full@(_ : _) = full


-- Ignore Comments
afterComment :: String -> String
afterComment []             = error "Unclosed Comment"
afterComment ('}' : source) = source
afterComment (_   : source) = afterComment source

----

bl :: Bool
bl = False
--bl = True

