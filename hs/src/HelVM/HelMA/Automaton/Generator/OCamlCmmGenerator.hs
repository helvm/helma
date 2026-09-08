module HelVM.HelMA.Automaton.Generator.OCamlCmmGenerator
  ( compileToCmm
  , generateCmm
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

-- | Główna funkcja generująca tekstowy format Cmm (OCaml IR)
compileToCmm ∷ InstructionList → Text
compileToCmm instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateCmm instructions

-- | Generuje nagłówek, obiekty globalne i główną funkcję w formacie Cmm
generateCmm ∷ InstructionList → Doc ann
generateCmm il = vsep
  [ ";; OCaml Cmm IR"
  , ";; Generowane automatycznie dla HelVM.HelMA.Automaton"
  , ""
  , ";; Zewnętrzne funkcje C dla I/O"
  , "(extcall \"putchar\" (val) -> val)"
  , "(extcall \"getchar\" () -> val)"
  , ""
  , ";; Globalna pamięć RAM (LSU) oraz Stos (ALU)"
  , "(data"
  , "  (align 8)"
  , "  (global \"caml_helma_ram\" (bytes 262144))  ;; 64k x 4-byte integers"
  , "  (global \"caml_helma_stack\" (bytes 262144))"
  , "  (global \"caml_helma_sp\" (int 0))"
  , ")"
  , ""
  , ";; Funkcja pomocnicza: push(val)"
  , "(function \"helma_push\" (v: val)"
  , "  (let (sp (load val \"caml_helma_sp\"))"
  , "    (store val (+ \"caml_helma_stack\" (* sp 4)) v)"
  , "    (store val \"caml_helma_sp\" (+ sp 1))))"
  , ""
  , ";; Funkcja pomocnicza: pop() -> val"
  , "(function \"helma_pop\" ()"
  , "  (let (sp (- (load val \"caml_helma_sp\") 1))"
  , "    (store val \"caml_helma_sp\" sp)"
  , "    (load val (+ \"caml_helma_stack\" (* sp 4)))))"
  , ""
  , ";; Główna funkcja wykonywalna"
  , "(function \"caml_helma_main\" ()"
  , "  (sequence"
  , "    " <> indent 4 (vsep (map genInstruction il))
  , "    0))"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na Cmm
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji dla ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i)) =
  "(app \"helma_push\" (" <> pretty i <> ") val)"
genSMInstruction (SPure (Unary LNot)) =
  "(app \"helma_push\" (== (app \"helma_pop\" () val) 0) val)"
genSMInstruction (SPure (Binary op)) = genBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map genBinOp ops)
genSMInstruction (SPure Discard) =
  "(let _ (app \"helma_pop\" () val) 0)"
genSMInstruction (SIO ioInst) = genIOInstruction ioInst
genSMInstruction inst = ";; Unsupported SMInstruction: " <> viaShow inst

-- | Translacja operacji binarnych ALU w Cmm
genBinOp ∷ BinaryOperation → Doc ann
genBinOp op =
  "(let (b (app \"helma_pop\" () val) a (app \"helma_pop\" () val))" <+>
    "(app \"helma_push\" (" <> cmmBinOp op <+> "a b) val))"

cmmBinOp ∷ BinaryOperation → Doc ann
cmmBinOp Add = "+"
cmmBinOp Sub = "-"
cmmBinOp Mul = "*"
cmmBinOp Div = "/"
cmmBinOp Mod = "mod"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load =
  "(let (addr (app \"helma_pop\" () val))" <+>
    "(app \"helma_push\" (load val (+ \"caml_helma_ram\" (* addr 4))) val))"
genLSInstruction Store =
  "(let (v (app \"helma_pop\" () val) addr (app \"helma_pop\" () val))" <+>
    "(store val (+ \"caml_helma_ram\" (* addr 4)) v))"
genLSInstruction RStore =
  "(let (addr (app \"helma_pop\" () val) v (app \"helma_pop\" () val))" <+>
    "(store val (+ \"caml_helma_ram\" (* addr 4)) v))"
genLSInstruction (LoadD a) =
  "(app \"helma_push\" (load val (+ \"caml_helma_ram\" " <> pretty (a * 4) <> ")) val)"
genLSInstruction (StoreI v) =
  "(let (addr (app \"helma_pop\" () val))" <+>
    "(store val (+ \"caml_helma_ram\" (* addr 4)) " <> pretty v <> "))"
genLSInstruction (RStoreD a) =
  "(let (v (app \"helma_pop\" () val))" <+>
    "(store val (+ \"caml_helma_ram\" " <> pretty (a * 4) <> ") v))"
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst = ";; Unsupported LSInstruction: " <> viaShow inst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l) =
  "(label \"label_" <> pretty l <> "\")"
genCFInstruction (Labeled Jump (LImmediate l)) =
  "(exit (label_" <> pretty l <> "))"
genCFInstruction Return =
  "(return 0)"
genCFInstruction (Branch test operand) = genBranchInstruction test operand
genCFInstruction inst = ";; Control Flow: " <> viaShow inst

genBranchInstruction ∷ BranchTest → BranchOperand → Doc ann
genBranchInstruction test (BImmediate l) =
  "(if (" <> cmmCmpPred test <+> "(app \"helma_pop\" () val) 0)" <+>
    "(exit (label_" <> pretty l <> ")) 0)"
genBranchInstruction _ op = ";; Dynamic Branch: " <> viaShow op

cmmCmpPred ∷ BranchTest → Doc ann
cmmCmpPred BEZ  = "=="
cmmCmpPred BNZ  = "!="
cmmCmpPred BLZ  = "<"
cmmCmpPred BGZ  = ">"
cmmCmpPred BLEZ = "<="
cmmCmpPred BGEZ = ">="

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar =
  "(extcall \"putchar\" ((app \"helma_pop\" () val)) val)"
genIOInstruction InputChar =
  "(app \"helma_push\" (extcall \"getchar\" () val) val)"
genIOInstruction _ = ";; Unsupported I/O"
