module HelVM.HelMA.Automaton.Generator.ForthGenerator
  ( compileToForth
  , generateForth
  ) where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Data.Text                                              ( Text )
import           Prettyprinter
import           Prettyprinter.Render.Text                              ( renderStrict )

-- | Główna funkcja generująca tekstowy format Forth (.fs)
compileToForth ∷ InstructionList → Text
compileToForth instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateForth instructions

-- | Generuje nagłówek, alokację RAM-u i strukturę skryptu Forth
generateForth ∷ InstructionList → Doc ann
generateForth il = vsep
  [ "( Generowane automatycznie przez HelVM.HelMA.Automaton.Generator.ForthGenerator )"
  , ""
  , "( Alokacja pamięci RAM: 65536 komórek 32-bitowych / cell )"
  , "create ram 65536 cells allot"
  , "ram 65536 cells erase"
  , ""
  , ": main"
  , "  " <> indent 2 (vsep (map genInstruction il))
  , "  ;"
  , ""
  , "main bye"
  , ""
  ]

-- | Translacja pojedynczej instrukcji z HelMA na Forth
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i))       = pretty i
genSMInstruction (SPure (Unary LNot))   = "0="
genSMInstruction (SPure (Binary op))    = forthBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map forthBinOp ops)
genSMInstruction (SPure Discard)        = "drop"
genSMInstruction (SPure Halibut)        = "rot"
genSMInstruction (SPure Pick)           = "pick"
genSMInstruction (SPure Roll)           = "roll"
genSMInstruction (SIO ioInst)           = genIOInstruction ioInst
genSMInstruction inst                   = "( Unsupported SMInstruction: " <> viaShow inst <> " )"

forthBinOp ∷ BinaryOperation → Doc ann
forthBinOp Add = "+"
forthBinOp Sub = "-"
forthBinOp Mul = "*"
forthBinOp Div = "/"
forthBinOp Mod = "mod"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = vsep
  [ "cells ram +"
  , "@"
  ]
genLSInstruction Store = vsep
  [ "swap"
  , "cells ram +"
  , "!"
  ]
genLSInstruction RStore = vsep
  [ "cells ram +"
  , "!"
  ]
genLSInstruction (LoadD a) = vsep
  [ pretty a <+> "cells ram +"
  , "@"
  ]
genLSInstruction (StoreI v) = vsep
  [ "cells ram +"
  , pretty v <+> "swap !"
  ]
genLSInstruction (RStoreD a) = vsep
  [ pretty a <+> "cells ram +"
  , "!"
  ]
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst = "( Unsupported LSInstruction: " <> viaShow inst <> " )"

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l) =
  "defer label_" <> pretty l
genCFInstruction (Labeled Jump (LImmediate l)) =
  "label_" <> pretty l
genCFInstruction Return =
  "exit"
genCFInstruction (Branch test (BImmediate l)) = vsep
  [ forthCmpPred test
  , "if label_" <> pretty l <+> "then"
  ]
genCFInstruction inst = "( Control Flow: " <> viaShow inst <> " )"

forthCmpPred ∷ BranchTest → Doc ann
forthCmpPred BEZ  = "0="
forthCmpPred BNZ  = "0<>"
forthCmpPred BLZ  = "0<"
forthCmpPred BGZ  = "0>"
forthCmpPred BLEZ = "0<="
forthCmpPred BGEZ = "0>="

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar = "emit"
genIOInstruction OutputDec  = ". cr"
genIOInstruction InputChar  = "key"
genIOInstruction _          = "( Unsupported I/O )"
