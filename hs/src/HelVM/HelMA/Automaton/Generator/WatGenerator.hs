module HelVM.HelMA.Automaton.Generator.WatGenerator
  ( compileToWat
  , generateWat
  ) where

import           HelVM.HelMA.Automaton.Instruction
import           HelVM.HelMA.Automaton.Instruction.Extras.Common
import           HelVM.HelMA.Automaton.Instruction.Groups.CFInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.IOInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.LSInstruction
import           HelVM.HelMA.Automaton.Instruction.Groups.SMInstruction

import           Data.Text                                              ( Text )
import qualified Data.Text                                              as T
import           Prettyprinter
import           Prettyprinter.Render.Text                              ( renderStrict )

-- | Główna funkcja generująca tekstowy format WebAssembly (WAT)
compileToWat ∷ InstructionList → Text
compileToWat instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateWat instructions

-- | Generuje strukturę modułu WAT z wygenerowanymi instrukcjami
generateWat ∷ InstructionList → Doc ann
generateWat il = vsep
  [ "(module"
  , "  ;; Importy dla I/O z otoczenia (np. JS / WASI)"
  , "  (import \"env\" \"putchar\" (func $putchar (param i32)))"
  , "  (import \"env\" \"getchar\" (func $getchar (result i32)))"
  , "  (import \"env\" \"putdec\"  (func $putdec  (param i32)))"
  , "  (import \"env\" \"getdec\"  (func $getdec  (result i32)))"
  , ""
  , "  ;; Pamięć RAM (LSU) - 1 strona = 64KB"
  , "  (memory $ram 1)"
  , "  (export \"memory\" (memory $ram))"
  , ""
  , "  (func $main (export \"main\")"
  , "    ;; Zmienne pomocnicze dla skomplikowanych operacji na stosie"
  , "    (local $tmp1 i32)"
  , "    (local $tmp2 i32)"
  , ""
  , "    " <> indent 4 (vsep (map genInstruction il))
  , "  )"
  , ")"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na WAT
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji dla ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i))                  = "i32.const" <+> pretty i
genSMInstruction (SPure (Unary LNot))              = vsep [ "i32.eqz" ]
genSMInstruction (SPure (Unary (UImmediate i op))) = vsep [ "i32.const" <+> pretty i, genBinOp op ]
genSMInstruction (SPure (Unary op))                = ";; Unsupported Unary:" <+> viaShow op
genSMInstruction (SPure (Binary op))               = genBinOp op
genSMInstruction (SPure (Binaries ops))            = vsep (map genBinOp ops)
genSMInstruction (SPure Discard)                   = "drop"
genSMInstruction (SPure Halibut)                   = ";; Halibut (Stack manipulation)"
genSMInstruction (SPure Pick)                      = ";; Pick (Stack manipulation)"
genSMInstruction (SPure Roll)                      = ";; Roll (Stack manipulation)"
genSMInstruction (SPure (Indexed _ _))             = ";; Indexed Stack Instruction"
genSMInstruction (SIO   ioInst)                    = genIOInstruction ioInst

-- | Translacja operacji binarnych ALU
genBinOp ∷ BinaryOperation → Doc ann
genBinOp Add = "i32.add"
genBinOp Sub = "i32.sub"
genBinOp Mul = "i32.mul"
genBinOp Div = "i32.div_s"
genBinOp Mod = "i32.rem_s"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = "i32.load" -- [Adres] -> [Wartość]
genLSInstruction Store = vsep
  [ ";; Store (value, address -> address, value)"
  , "local.set $tmp1" -- value
  , "local.set $tmp2" -- address
  , "local.get $tmp2"
  , "local.get $tmp1"
  , "i32.store"
  ]
genLSInstruction RStore = "i32.store" -- [Adres, Wartość] -> 1:1 z WASM!
genLSInstruction (LoadD a) = vsep
  [ "i32.const" <+> pretty a
  , "i32.load"
  ]
genLSInstruction (StoreI v) = vsep
  [ "local.set $tmp2" -- address
  , "i32.const" <+> pretty v
  , "local.get $tmp2"
  , "i32.store"
  ]
genLSInstruction (RStoreD a) = vsep
  [ "i32.const" <+> pretty a
  , "i32.store"
  ]
genLSInstruction (StoreID v a) = vsep
  [ "i32.const" <+> pretty a
  , "i32.const" <+> pretty v
  , "i32.store"
  ]
genLSInstruction (MoveD src dst) = vsep
  [ "i32.const" <+> pretty dst
  , "i32.const" <+> pretty src
  , "i32.load"
  , "i32.store"
  ]
genLSInstruction (MIO ioInst) = genIOInstruction ioInst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l)                       = "block $label_" <> pretty l <> " ;; Mark"
genCFInstruction (Labeled Jump (LImmediate l))  = "br $label_" <> pretty l
genCFInstruction (Labeled Jump (LArtificial l)) = "br $art_label_" <> pretty l
genCFInstruction (Labeled Call (LImmediate l))  = "call $func_" <> pretty l
genCFInstruction Return                         = "return"
genCFInstruction (Branch test operand)          = genBranchInstruction test operand
genCFInstruction inst                           = ";; Control Flow:" <+> viaShow inst

genBranchInstruction ∷ BranchTest → BranchOperand → Doc ann
genBranchInstruction test (BImmediate l) = vsep
  [ genBranchTest test
  , "br_if $label_" <> pretty l
  ]
genBranchInstruction _ op = ";; Dynamic Branch:" <+> viaShow op

genBranchTest ∷ BranchTest → Doc ann
genBranchTest BEZ  = "i32.eqz"
genBranchTest BNZ  = "i32.eqz\ni32.eqz"
genBranchTest BLZ  = "i32.const 0\ni32.lt_s"
genBranchTest BGZ  = "i32.const 0\ni32.gt_s"
genBranchTest BLEZ = "i32.const 0\ni32.le_s"
genBranchTest BGEZ = "i32.const 0\ni32.ge_s"

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar      = "call $putchar"
genIOInstruction OutputDec       = "call $putdec"
genIOInstruction OutputCharMaybe = "call $putchar"
genIOInstruction OutputDecMaybe  = "call $putdec"
genIOInstruction InputChar       = "call $getchar"
genIOInstruction InputDec        = "call $getdec"
