module HelVM.HelMA.Automaton.Generator.GhcCmmGenerator
  ( compileToGhcCmm
  , generateGhcCmm
  ) where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Prettyprinter
import           Prettyprinter.Render.Text                              ( renderStrict )

-- | Główna funkcja generująca tekstowy format GHC Cmm (.cmm)
compileToGhcCmm ∷ InstructionList → Text
compileToGhcCmm instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateGhcCmm instructions

-- | Generuje struktury nagłówkowe i procedury dla GHC RTS
generateGhcCmm ∷ InstructionList → Doc ann
generateGhcCmm il = vsep
  [ "#include \"Cmm.h\""
  , ""
  , ";; Pamięć RAM (LSU) oraz Stos (ALU) w pamięci statycznej GHC RTS"
  , "section \"data\" {"
  , "  align 8;"
  , "  helma_ram:"
  , "    skip 262144; ;; 65536 * 4 bytes"
  , "  helma_stack:"
  , "    skip 262144;"
  , "  helma_sp:"
  , "    W_ 0;"
  , "}"
  , ""
  , ";; Pomocnicza funkcja Push"
  , "helma_push(W_ val) {"
  , "  W_ sp;"
  , "  sp = W_[helma_sp];"
  , "  W_[helma_stack + sp * 4] = val;"
  , "  W_[helma_sp] = sp + 1;"
  , "  return ();"
  , "}"
  , ""
  , ";; Pomocnicza funkcja Pop"
  , "helma_pop() {"
  , "  W_ sp;"
  , "  sp = W_[helma_sp] - 1;"
  , "  W_[helma_sp] = sp;"
  , "  return (W_[helma_stack + sp * 4]);"
  , "}"
  , ""
  , ";; Główna procedura wywoływalna z poziomu Haskella przez FFI/Cmm"
  , "helma_cmm_main() {"
  , "  " <> indent 2 (vsep (map genInstruction il))
  , "  return (0);"
  , "}"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na GHC Cmm
genInstruction ∷ Instruction → Doc ann
genInstruction (ISM inst) = genSMInstruction inst
genInstruction (ILS inst) = genLSInstruction inst
genInstruction (ICF inst) = genCFInstruction inst
genInstruction End        = "return (0);"

-- | 1. Generowanie instrukcji dla ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i)) =
  "call helma_push(" <> pretty i <> ");"
genSMInstruction (SPure (Unary LNot)) = vsep
  [ "W_ a;"
  , "(a) = call helma_pop();"
  , "call helma_push(a == 0);"
  ]
genSMInstruction (SPure (Binary op))    = genBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map genBinOp ops)
genSMInstruction (SPure Discard) = vsep
  [ "W_ unused;"
  , "(unused) = call helma_pop();"
  ]
genSMInstruction (SIO ioInst) = genIOInstruction ioInst
genSMInstruction inst         = ";; Unsupported SMInstruction: " <> viaShow inst

genBinOp ∷ BinaryOperation → Doc ann
genBinOp op = vsep
  [ "W_ a, b;"
  , "(b) = call helma_pop();"
  , "(a) = call helma_pop();"
  , "call helma_push(a " <> cmmBinOp op <> " b);"
  ]

cmmBinOp ∷ BinaryOperation → Doc ann
cmmBinOp Add  = "+"
cmmBinOp Sub  = "-"
cmmBinOp Mul  = "*"
cmmBinOp Div  = "/"
cmmBinOp Mod  = "%"
cmmBinOp BAnd = "&"
cmmBinOp BOr  = "|"
cmmBinOp BXor = "^"
cmmBinOp op   = ";; Unsupported BinOp: " <> viaShow op

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = vsep
  [ "W_ addr;"
  , "(addr) = call helma_pop();"
  , "call helma_push(W_[helma_ram + addr * 4]);"
  ]
genLSInstruction Store = vsep
  [ "W_ val, addr;"
  , "(val) = call helma_pop();"
  , "(addr) = call helma_pop();"
  , "W_[helma_ram + addr * 4] = val;"
  ]
genLSInstruction RStore = vsep
  [ "W_ addr, val;"
  , "(addr) = call helma_pop();"
  , "(val) = call helma_pop();"
  , "W_[helma_ram + addr * 4] = val;"
  ]
genLSInstruction (LoadD a) =
  "call helma_push(W_[helma_ram + " <> pretty (a * 4) <> "]);"
genLSInstruction (StoreI v) = vsep
  [ "W_ addr;"
  , "(addr) = call helma_pop();"
  , "W_[helma_ram + addr * 4] = " <> pretty v <> ";"
  ]
genLSInstruction (RStoreD a) = vsep
  [ "W_ val;"
  , "(val) = call helma_pop();"
  , "W_[helma_ram + " <> pretty (a * 4) <> "] = val;"
  ]
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst         = ";; Unsupported LSInstruction: " <> viaShow inst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark (MNatural l)) =
  "label_" <> pretty l <> ":"
genCFInstruction (Mark (MArtificial l)) =
  "label_" <> viaShow l <> ":"
genCFInstruction (Labeled (LImmediate l) Jump) =
  "goto label_" <> pretty l <> ";"
genCFInstruction (Labeled (LArtificial l) Jump) =
  "goto label_" <> viaShow l <> ";"
genCFInstruction Return =
  "return (0);"
genCFInstruction (Branch (BImmediate l) test) = vsep
  [ "W_ cond;"
  , "(cond) = call helma_pop();"
  , "if (cond " <> cmmCmpPred test <> " 0) { goto label_" <> pretty l <> "; }"
  ]
genCFInstruction (Branch (BArtificial l) test) = vsep
  [ "W_ cond;"
  , "(cond) = call helma_pop();"
  , "if (cond " <> cmmCmpPred test <> " 0) { goto label_" <> viaShow l <> "; }"
  ]
genCFInstruction inst = ";; Control Flow: " <> viaShow inst

cmmCmpPred ∷ BranchTest → Doc ann
cmmCmpPred EZ  = "=="
cmmCmpPred NE  = "!="
cmmCmpPred LTZ = "<"
cmmCmpPred GTZ = ">"

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar = vsep
  [ "W_ c;"
  , "(c) = call helma_pop();"
  , "foreign \"C\" putchar(c \"signed\");"
  ]
genIOInstruction InputChar = vsep
  [ "W_ c;"
  , "(c) = foreign \"C\" getchar();"
  , "call helma_push(c);"
  ]
genIOInstruction _ = ";; Unsupported I/O"
