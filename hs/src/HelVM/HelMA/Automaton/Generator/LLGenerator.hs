module HelVM.HelMA.Automaton.Generator.LLGenerator
  ( compileToLL
  , generateLL
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

-- | Główna funkcja generująca tekstowy reprezentacyjny kod LLVM IR (.ll)
compileToLL ∷ InstructionList → Text
compileToLL instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateLL instructions

-- | Generuje nagłówek, definicje funkcji i strukturę LLVM IR
generateLL ∷ InstructionList → Doc ann
generateLL il = vsep
  [ ";; Target Triple & Data Layout (Standard x86_64 / ARM64)"
  , ";; Generowane automatycznie przez HelVM.HelMA.Automaton.Generator.LLGenerator"
  , ""
  , ";; Deklaracje funkcji I/O ze standardowej biblioteki C"
  , "declare i32 @putchar(i32)"
  , "declare i32 @getchar()"
  , "declare i32 @printf(i8*, ...)"
  , "declare i32 @scanf(i8*, ...)"
  , ""
  , ";; Pamięć RAM (LSU) oraz Stos Operacyjny (ALU)"
  , "@ram = global [65536 x i32] zeroinitializer, align 16"
  , "@stack = global [65536 x i32] zeroinitializer, align 16"
  , "@sp = global i32 0, align 4 ;; Stack Pointer"
  , ""
  , "@dec_fmt_out = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1"
  , "@dec_fmt_in  = private unnamed_addr constant [3 x i8] c\"%d\\00\", align 1"
  , ""
  , ";; Funkcje pomocnicze dla operacji stosowych (Push / Pop)"
  , "define void @push(i32 %val) {"
  , "  %sp_val = load i32, i32* @sp"
  , "  %ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @stack, i32 0, i32 %sp_val"
  , "  store i32 %val, i32* %ptr"
  , "  %next_sp = add i32 %sp_val, 1"
  , "  store i32 %next_sp, i32* @sp"
  , "  ret void"
  , "}"
  , ""
  , "define i32 @pop() {"
  , "  %sp_val = load i32, i32* @sp"
  , "  %prev_sp = sub i32 %sp_val, 1"
  , "  store i32 %prev_sp, i32* @sp"
  , "  %ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @stack, i32 0, i32 %prev_sp"
  , "  %val = load i32, i32* %ptr"
  , "  ret i32 %val"
  , "}"
  , ""
  , "define i32 @main() {"
  , "entry:"
  , "  br label %block_0"
  , ""
  , "block_0:"
  , "  " <> indent 2 (vsep (map genInstruction il))
  , "  ret i32 0"
  , "}"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na LLVM IR
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i)) =
  "call void @push(i32 " <> pretty i <> ")"
genSMInstruction (SPure (Unary LNot)) = vsep
  [ "%a = call i32 @pop()"
  , "%cmp = icmp eq i32 %a, 0"
  , "%res = zext i1 %cmp to i32"
  , "call void @push(i32 %res)"
  ]
genSMInstruction (SPure (Binary op)) = genBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map genBinOp ops)
genSMInstruction (SPure Discard) =
  "%unused = call i32 @pop()"
genSMInstruction (SIO ioInst) = genIOInstruction ioInst
genSMInstruction inst = ";; Unsupported SMInstruction: " <> viaShow inst

-- | Translacja operacji binarnych ALU
genBinOp ∷ BinaryOperation → Doc ann
genBinOp op = vsep
  [ "%b = call i32 @pop()"
  , "%a = call i32 @pop()"
  , "%res = " <> llvmBinOp op <> " i32 %a, %b"
  , "call void @push(i32 %res)"
  ]

llvmBinOp ∷ BinaryOperation → Doc ann
llvmBinOp Add = "add"
llvmBinOp Sub = "sub"
llvmBinOp Mul = "mul"
llvmBinOp Div = "sdiv"
llvmBinOp Mod = "srem"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = vsep
  [ "%addr = call i32 @pop()"
  , "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 %addr"
  , "%val = load i32, i32* %ram_ptr"
  , "call void @push(i32 %val)"
  ]
genLSInstruction Store = vsep
  [ ";; Store (value, address)"
  , "%val = call i32 @pop()"
  , "%addr = call i32 @pop()"
  , "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 %addr"
  , "store i32 %val, i32* %ram_ptr"
  ]
genLSInstruction RStore = vsep
  [ ";; RStore (address, value)"
  , "%addr = call i32 @pop()"
  , "%val = call i32 @pop()"
  , "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 %addr"
  , "store i32 %val, i32* %ram_ptr"
  ]
genLSInstruction (LoadD a) = vsep
  [ "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 " <> pretty a
  , "%val = load i32, i32* %ram_ptr"
  , "call void @push(i32 %val)"
  ]
genLSInstruction (StoreI v) = vsep
  [ "%addr = call i32 @pop()"
  , "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 %addr"
  , "store i32 " <> pretty v <> ", i32* %ram_ptr"
  ]
genLSInstruction (RStoreD a) = vsep
  [ "%val = call i32 @pop()"
  , "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 " <> pretty a
  , "store i32 %val, i32* %ram_ptr"
  ]
genLSInstruction (StoreID v a) = vsep
  [ "%ram_ptr = getelementptr inbounds [65536 x i32], [65536 x i32]* @ram, i32 0, i32 " <> pretty a
  , "store i32 " <> pretty v <> ", i32* %ram_ptr"
  ]
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst = ";; Unsupported LSInstruction: " <> viaShow inst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l) = vsep
  [ "br label %label_" <> pretty l
  , "label_" <> pretty l <> ":"
  ]
genCFInstruction (Labeled Jump (LImmediate l)) =
  "br label %label_" <> pretty l
genCFInstruction (Labeled Jump (LArtificial l)) =
  "br label %art_label_" <> pretty l
genCFInstruction Return =
  "ret i32 0"
genCFInstruction (Branch test operand) = genBranchInstruction test operand
genCFInstruction inst = ";; Control Flow: " <> viaShow inst

genBranchInstruction ∷ BranchTest → BranchOperand → Doc ann
genBranchInstruction test (BImmediate l) = vsep
  [ "%cond_val = call i32 @pop()"
  , "%cond = icmp " <> llvmCmpPred test <> " i32 %cond_val, 0"
  , "%tmp_label = select i1 %cond, label %label_" <> pretty l <> ", label %next_label"
  , "br label %tmp_label"
  , "next_label:"
  ]
genBranchInstruction _ op = ";; Dynamic Branch: " <> viaShow op

llvmCmpPred ∷ BranchTest → Doc ann
llvmCmpPred BEZ  = "eq"
llvmCmpPred BNZ  = "ne"
llvmCmpPred BLZ  = "slt"
llvmCmpPred BGZ  = "sgt"
llvmCmpPred BLEZ = "sle"
llvmCmpPred BGEZ = "sge"

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar = vsep
  [ "%char_val = call i32 @pop()"
  , "call i32 @putchar(i32 %char_val)"
  ]
genIOInstruction OutputDec = vsep
  [ "%dec_val = call i32 @pop()"
  , "%fmt_ptr = getelementptr inbounds [4 x i8], [4 x i8]* @dec_fmt_out, i32 0, i32 0"
  , "call i32 (i8*, ...) @printf(i8* %fmt_ptr, i32 %dec_val)"
  ]
genIOInstruction InputChar = vsep
  [ "%in_char = call i32 @getchar()"
  , "call void @push(i32 %in_char)"
  ]
genIOInstruction _ = ";; Unsupported I/O Instruction"
