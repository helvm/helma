module HelVM.HelMA.Automata.Piet.Instruction where

fromMixedColors :: MixedColor -> MixedColor -> Int -> Instruction
fromMixedColors c1 c2 = fromMixedColor (colorChange c1 c2)

--TODO it is posible to use Free Monad here ?
fromMixedColor :: MixedColor -> Int -> Instruction
fromMixedColor (MixedColor Light  Red    ) _ = stop
fromMixedColor (MixedColor Normal Red    ) n = push n
fromMixedColor (MixedColor Dark   Red    ) _ = pop
fromMixedColor (MixedColor Light  Yellow ) _ = add
fromMixedColor (MixedColor Normal Yellow ) _ = subtract
fromMixedColor (MixedColor Dark   Yellow ) _ = multiply
fromMixedColor (MixedColor Light  Green  ) _ = divide
fromMixedColor (MixedColor Normal Green  ) _ = mod
fromMixedColor (MixedColor Dark   Green  ) _ = not
fromMixedColor (MixedColor Light  Cyan   ) _ = greater
fromMixedColor (MixedColor Normal Cyan   ) _ = pointer
fromMixedColor (MixedColor Dark   Cyan   ) _ = switch
fromMixedColor (MixedColor Light  Blue   ) _ = duplicate
fromMixedColor (MixedColor Normal Blue   ) _ = roll
fromMixedColor (MixedColor Dark   Blue   ) _ = inNumber
fromMixedColor (MixedColor Light  Magenta) _ = inChar
fromMixedColor (MixedColor Normal Magenta) _ = outNumber
fromMixedColor (MixedColor Dark   Magenta) _ = outChar
