module HelVM.HelMA.Automaton.Generator.HolyCGenerator
  ( compileToHolyC
  , generateHolyC
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

-- | Główna funkcja generująca kod w języku HolyC (TempleOS)
compileToHolyC ∷ InstructionList → Text
compileToHolyC instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateHolyC instructions

-- | Generuje strukturę kodu HolyC
generateHolyC ∷ InstructionList → Doc ann
generateHolyC il = vsep
  [ "// Generowane automatycznie dla TempleOS / HolyC"
  , "// HelVM.HelMA.Automaton.Generator.HolyCGenerator"
  , ""
  , "I64 ram[65536];"
  , "MemSet(ram, 0, sizeof(ram));"
  , ""
  , "I64 stack[65536];"
  , "I64 sp = 0;"
  , ""
  , "U0 Push(I64 val)"
  , "{"
  , "  stack[sp++] = val;"
  , "}"
  , ""
  , "I64 Pop()"
  , "{"
  , "  return stack[--sp];"
  , "}"
  , ""
  , "U0 Main()"
  , "{"
  , "  " <> indent 2 (vsep (map genInstruction il))
  , "}"
  , ""
  , "Main; // W HolyC odpalenie funkcji następuje po prostu przez jej wywołanie w skrypcie"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na HolyC
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i))       = "Push(" <> pretty i <> ");"
genSMInstruction (SPure (Unary LNot))   = "Push(Pop == 0);"
genSMInstruction (SPure (Binary op))    = genBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map genBinOp ops)
genSMInstruction (SPure Discard)        = "Pop;"
genSMInstruction (SIO ioInst)           = genIOInstruction ioInst
genSMInstruction inst                   = "// Unsupported SMInstruction: " <> viaShow inst

genBinOp ∷ BinaryOperation → Doc ann
genBinOp op = vsep
  [ "{"
  , "  I64 b = Pop;"
  , "  I64 a = Pop;"
  , "  Push(a " <> holyBinOp op <> " b);"
  , "}"
  ]

holyBinOp ∷ BinaryOperation → Doc ann
holyBinOp Add = "+"
holyBinOp Sub = "-"
holyBinOp Mul = "*"
holyBinOp Div = "/"
holyBinOp Mod = "%"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = "Push(ram[Pop]);"
genLSInstruction Store = vsep
  [ "{"
  , "  I64 val = Pop;"
  , "  I64 addr = Pop;"
  , "  ram[addr] = val;"
  , "}"
  ]
genLSInstruction RStore = vsep
  [ "{"
  , "  I64 addr = Pop;"
  , "  I64 val = Pop;"
  , "  ram[addr] = val;"
  , "}"
  ]
genLSInstruction (LoadD a) = "Push(ram[" <> pretty a <> "]);"
genLSInstruction (StoreI v) = "ram[Pop] = " <> pretty v <> ";"
genLSInstruction (RStoreD a) = "ram[" <> pretty a <> "] = Pop;"
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst = "// Unsupported LSInstruction: " <> viaShow inst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l) = "label_" <> pretty l <> ":"
genCFInstruction (Labeled Jump (LImmediate l)) = "goto label_" <> pretty l <> ";"
genCFInstruction Return = "return;"
genCFInstruction (Branch test (BImmediate l)) =
  "if (Pop " <> holyCmpPred test <> " 0) goto label_" <> pretty l <> ";"
genCFInstruction inst = "// Control Flow: " <> viaShow inst

holyCmpPred ∷ BranchTest → Doc ann
holyCmpPred BEZ  = "=="
holyCmpPred BNZ  = "!="
holyCmpPred BLZ  = "<"
holyCmpPred BGZ  = ">"
holyCmpPred BLEZ = "<="
holyCmpPred BGEZ = ">="

-- | 4. Instrukcje WE/WY (I/O w HolyC)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar = "Print(\"%c\", Pop);"
genIOInstruction OutputDec  = "Print(\"%d\\n\", Pop);"
genIOInstruction InputChar  = "Push(GetChar);"
genIOInstruction _          = "// Unsupported I/O"
