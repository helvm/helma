{-# LANGUAGE TupleSections     #-}
module HelVM.HelMA.Automata.Piet.LLVM.Internal.Runtime
  ( add
  , declareFunctions
  , divide
  , duplicate
  , greater
  , inChar
  , inNumber
  , mod
  , multiply
  , not
  , outChar
  , outNumber
  , pointer
  , pop
  , push
  , resetStack
  , roll
  , runtimeAssembly
  , subtract
  , switch
  ) where

import           Control.Monad
import           Data.ByteString   ( ByteString )
import           Data.FileEmbed
import qualified LLVM.AST          as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global   as G
import qualified LLVM.AST.Type     as T
import           LLVM.IRBuilder    hiding ( add, switch )
import           Prelude           hiding ( mod, not, subtract )

runtimeAssembly ∷ ByteString
runtimeAssembly = $(embedFile "src/Language/Piet/Internal/runtime.ll")

declareFunctions ∷ MonadModuleBuilder m ⇒ m ()
declareFunctions = do
  let noArgsCommands = [ "pop"
                       , "add"
                       , "subtract"
                       , "multiply"
                       , "divide"
                       , "mod"
                       , "not"
                       , "greater"
                       , "duplicate"
                       , "roll"
                       , "in_number"
                       , "in_char"
                       , "out_number"
                       , "out_char"
                       ]
  declareFunction "push" T.void [T.i32]
  declareFunction "pointer" T.void [T.ptr T.i32]
  declareFunction "switch" T.void [T.ptr T.i32]
  mapM_ (\name -> declareFunction name T.void []) noArgsCommands
  declareFunction "reset_stack" T.void []

resetStack ∷ MonadIRBuilder m ⇒ m ()
resetStack = callNoArgsVoidFunction "reset_stack"

push ∷ MonadIRBuilder m ⇒ Int → m ()
push n = callVoidFunction "push" [T.i32] [AST.ConstantOperand $ C.Int 32 $ toInteger n]

pop ∷ MonadIRBuilder m ⇒ m ()
pop = callNoArgsVoidFunction "pop"

add ∷ MonadIRBuilder m ⇒ m ()
add = callNoArgsVoidFunction "add"

subtract ∷ MonadIRBuilder m ⇒ m ()
subtract = callNoArgsVoidFunction "subtract"

multiply ∷ MonadIRBuilder m ⇒ m ()
multiply = callNoArgsVoidFunction "multiply"

divide ∷ MonadIRBuilder m ⇒ m ()
divide = callNoArgsVoidFunction "divide"

mod ∷ MonadIRBuilder m ⇒ m ()
mod = callNoArgsVoidFunction "mod"

not ∷ MonadIRBuilder m ⇒ m ()
not = callNoArgsVoidFunction "not"

greater ∷ MonadIRBuilder m ⇒ m ()
greater = callNoArgsVoidFunction "greater"

pointer ∷ MonadIRBuilder m ⇒ AST.Operand → m ()
pointer p = callVoidFunction "pointer" [T.ptr T.i32] [p]

switch ∷ MonadIRBuilder m ⇒ AST.Operand → m ()
switch p = callVoidFunction "switch" [T.ptr T.i32] [p]

duplicate ∷ MonadIRBuilder m ⇒ m ()
duplicate = callNoArgsVoidFunction "duplicate"

roll ∷ MonadIRBuilder m ⇒ m ()
roll = callNoArgsVoidFunction "roll"

inNumber ∷ MonadIRBuilder m ⇒ m ()
inNumber = callNoArgsVoidFunction "in_number"

inChar ∷ MonadIRBuilder m ⇒ m ()
inChar = callNoArgsVoidFunction "in_char"

outNumber ∷ MonadIRBuilder m ⇒ m ()
outNumber = callNoArgsVoidFunction "out_number"

outChar ∷ MonadIRBuilder m ⇒ m ()
outChar = callNoArgsVoidFunction "out_char"

declareFunction ∷ MonadModuleBuilder m ⇒ AST.Name → AST.Type → [AST.Type] → m ()
declareFunction name returnType argumentTypes = do
  let params = zipWith (\t n -> G.Parameter t (AST.UnName n) []) argumentTypes [0..]
  let globalDef = G.functionDefaults { G.name = name
                                     , G.returnType = returnType
                                     , G.parameters = (params, False)
                                     }
  emitDefn $ AST.GlobalDefinition globalDef
  return ()

callNoArgsVoidFunction ∷ MonadIRBuilder m ⇒ AST.Name → m ()
callNoArgsVoidFunction name = callVoidFunction name [] []

callVoidFunction ∷ MonadIRBuilder m ⇒ AST.Name → [AST.Type] → [AST.Operand] → m ()
callVoidFunction name argumentTypes args = void $ callFunction name T.void argumentTypes args

callFunction ∷ MonadIRBuilder m ⇒ AST.Name → AST.Type → [AST.Type] → [AST.Operand] → m AST.Operand
callFunction name returnType argumentTypes args = do
  let functionType = T.FunctionType returnType argumentTypes False
  let functionOperand = AST.ConstantOperand $ C.GlobalReference (T.ptr functionType) name
  call functionOperand $ (, []) <$> args
