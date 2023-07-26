module HelVM.HelMA.Automata.Piet.Instruction where

import           HelVM.HelMA.Automata.Piet.MixedColor

--import           HelVM.HelMA.Automata.Piet.Pair

--fromMixedColors :: Pair MixedColor -> Int -> Instruction
--fromMixedColors p = fromMixedColor (colorChange p)

----TODO it is possible to use Free Monad here ?
fromMixedColor :: MixedColor -> Int -> Instruction
fromMixedColor (MixedColor Light  Red    ) _ = stop
fromMixedColor (MixedColor Normal Red    ) n = push n
fromMixedColor (MixedColor Dark   Red    ) _ = pop
fromMixedColor (MixedColor Light  Yellow ) _ = add
fromMixedColor (MixedColor Normal Yellow ) _ = sub
fromMixedColor (MixedColor Dark   Yellow ) _ = multiply
fromMixedColor (MixedColor Light  Green  ) _ = divide
fromMixedColor (MixedColor Normal Green  ) _ = mod_
fromMixedColor (MixedColor Dark   Green  ) _ = not_
fromMixedColor (MixedColor Light  Cyan   ) _ = greater
fromMixedColor (MixedColor Normal Cyan   ) _ = pointer
fromMixedColor (MixedColor Dark   Cyan   ) _ = switch
fromMixedColor (MixedColor Light  Blue   ) _ = duplicate
fromMixedColor (MixedColor Normal Blue   ) _ = roll
fromMixedColor (MixedColor Dark   Blue   ) _ = inNumber
fromMixedColor (MixedColor Light  Magenta) _ = inChar
fromMixedColor (MixedColor Normal Magenta) _ = outNumber
fromMixedColor (MixedColor Dark   Magenta) _ = outChar

push :: Int -> Instruction
push = Push

stop , pop :: Instruction
add , sub , multiply , divide , mod_ , not_ , greater :: Instruction
pointer , switch , duplicate , roll :: Instruction
inNumber , inChar , outNumber , outChar :: Instruction

stop = Stop
pop = Pop
add = Add
sub = Sub
multiply = Stop
divide = Stop
mod_ = Mod
not_ = Not
greater = Gt
pointer = Pointer
switch = Switch
duplicate = Dup
roll = Rol
inNumber = InN
inChar = InC
outNumber = OutN
outChar = OutC

data Instruction =
    Stop | Push Int | Pop
  | Add | Sub | Mul | Div | Mod
  | Not | Gt
  | Pointer | Switch
  | Dup | Rol
  | InN | InC | OutN | OutC
