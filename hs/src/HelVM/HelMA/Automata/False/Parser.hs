module HelVM.HelMA.Automata.False.Parser where

import           HelVM.HelMA.Automata.False.Util

import           HelVM.HelIO.ReadText

import           Data.Char

import           HelVM.HelIO.Control.Safe

parseFromTextSafe :: Text -> Safe [Value]
parseFromTextSafe = pure . parse . toString

parse :: [Char] -> [Value]
parse [] = []
-- Parse Arithmetic
parse ('+':prog) = (Ope Plus):(parse prog)
parse ('-':prog) = (Ope Minus):(parse prog)
parse ('*':prog) = (Ope Multiply):(parse prog)
parse ('/':prog) = (Ope Division):(parse prog)
parse ('_':prog) = (Ope Negate):(parse prog)

-- Parse Comparison
parse ('=':prog) = (Ope Equal):(parse prog)
parse ('>':prog) = (Ope Greater):(parse prog)

-- Parse Boolean Algebra
parse ('&':prog) = (Ope And):(parse prog)
parse ('|':prog) = (Ope Or):(parse prog)
parse ('~':prog) = (Ope Not):(parse prog)

-- Parse Stack Ope
parse ('$':prog) = (Ope Dup):(parse prog)
parse ('%':prog) = (Ope Drop):(parse prog)
parse ('\\':prog) = (Ope Swap):(parse prog)
parse ('@':prog) = (Ope Rot):(parse prog)
parse ('o':prog) = (Ope Pick):(parse prog)

-- Parse Control Flow
parse ('!':prog) = (Ope Execute):(parse prog)
parse ('?':prog) = (Ope If):(parse prog)
parse ('#':prog) = (Ope While):(parse prog)
parse ('{':prog) = (parse (afterComment prog))
parse ('[':prog) = (Fn (parse $ parseLambda 0 prog)):(parse $ afterLambda 0 prog)

-- Parse Standard Input/Output
parse ('^':prog) = (Io ReadChar):(parse prog)
parse (',':prog) = (Io WriteChar):(parse prog)
parse ('.':prog) = (Io WriteDec):(parse prog)
parse ('"':prog) = (Io (Str (parseString prog))):(parse $ afterString prog)

-- Parse Variables
parse (c:prog) | isAlpha c = (Var (charToVar c)):(parse prog)
parse (':':prog) = (Ope Store):(parse prog)
parse (';':prog) = (Ope Fetch):(parse prog)

-- Parse Literals
parse ('\'':c:prog) = (Ch c):(parse prog)
parse full@(i:prog) | isDigit i = (Num (readTextUnsafe $ toText $ parseNum full)):(parse (afterNum prog))

-- Skip other shit
parse (_:prog) = (parse prog)


-- Parse manipulators
-- Parse Strings
parseString :: [Char] -> [Char]
parseString ('"':_)         = []
-- Handle special characters explicitly
parseString ('\\':'a':prog) = (chr 7):(parseString prog)
parseString ('\\':'b':prog) = (chr 8):(parseString prog)
parseString ('\\':'f':prog) = (chr 12):(parseString prog)
parseString ('\\':'n':prog) = (chr 10):(parseString prog)
parseString ('\\':'r':prog) = (chr 13):(parseString prog)
parseString ('\\':'t':prog) = (chr 9):(parseString prog)
parseString ('\\':'"':prog) = (chr 34):(parseString prog)
parseString (c:prog)        = c:(parseString prog)
parseString []              = error "Empty"

afterString :: [Char] -> [Char]
afterString ('"' : prog) = prog
afterString (_   : prog) = afterString prog
afterString []           = error "Empty"


-- Parse Lambdas
parseLambda :: Int -> [Char] -> [Char]
parseLambda _ []         = error "Unclosed lambda"
parseLambda 0 (']':_)    = []
parseLambda n (']':prog) = ']':(parseLambda (n-1) prog)
parseLambda n ('[':prog) = '[':(parseLambda (n+1) prog)
parseLambda n (c:prog)   = c:(parseLambda n prog)

afterLambda :: Int -> [Char] -> [Char]
afterLambda _ []         = error "Unclosed lambda"
afterLambda 0 (']':prog) = prog
afterLambda n (']':prog) = afterLambda (n-1) prog
afterLambda n ('[':prog) = afterLambda (n+1) prog
afterLambda n (_:prog)   = afterLambda n prog

-- Parse Numbers
parseNum :: [Char] -> [Char]
parseNum [] = []
parseNum ('.':i:prog) | isDigit i = '.':i:(parseNum prog)
parseNum (i:prog) | isDigit i = i:(parseNum prog)
parseNum (_ : _) = []

afterNum :: [Char] -> [Char]
afterNum [] = []
afterNum (i:prog) | isDigit i = afterNum prog
afterNum full@(_:_) = full


-- Ignore Comments
afterComment :: [Char] -> [Char]
afterComment []         = error "Unclosed Comment"
afterComment ('}':prog) = prog
afterComment (_:prog)   = afterComment prog
