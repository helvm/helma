module HelVM.HelMA.Automaton.Generator.JasminGenerator
  ( compileToJasmin
  , generateJasmin
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

-- | Główna funkcja generująca tekstowy format Jasmin Assembler (.j)
compileToJasmin ∷ InstructionList → Text
compileToJasmin instructions = renderStrict . layoutPretty defaultLayoutOptions $ generateJasmin instructions

-- | Generuje strukturę klasy JVM i metody main
generateJasmin ∷ InstructionList → Doc ann
generateJasmin il = vsep
  [ ".class public Main"
  , ".super java/lang/Object"
  , ""
  , "; Pamięć RAM (LSU) oraz Stos operacyjny"
  , ".field static ram [I"
  , ""
  , ".method public <init>()V"
  , "   aload_0"
  , "   invokespecial java/lang/Object/<init>()V"
  , "   return"
  , ".end method"
  , ""
  , ".method public static main([Ljava/lang/String;)V"
  , "   .limit stack 1000"
  , "   .limit locals 10"
  , ""
  , "   ; Alokacja pamięci RAM: 65536 intów"
  , "   sipush 65536"
  , "   newarray int"
  , "   putstatic Main/ram [I"
  , ""
  , "   " <> indent 3 (vsep (map genInstruction il))
  , ""
  , "   return"
  , ".end method"
  ]

-- | Translacja pojedynczej instrukcji z HelMA na bajtokod JVM
genInstruction ∷ Instruction → Doc ann
genInstruction (SInstruction   inst) = genSMInstruction inst
genInstruction (LSInstruction  inst) = genLSInstruction inst
genInstruction (CFInstruction  inst) = genCFInstruction inst

-- | 1. Generowanie instrukcji ALU / Stosu (SMInstruction)
genSMInstruction ∷ SMInstruction → Doc ann
genSMInstruction (SPure (Cons i)) =
  "ldc " <> pretty i
genSMInstruction (SPure (Unary LNot)) = vsep
  [ "ifne label_not_zero_" <> pretty i
  , "iconst_1"
  , "goto label_not_end_" <> pretty i
  , "label_not_zero_" <> pretty i <> ":"
  , "iconst_0"
  , "label_not_end_" <> pretty i <> ":"
  ] where i = 0 :: Int -- tymczasowa etykieta pomocnicza dla LNot
genSMInstruction (SPure (Binary op)) = genBinOp op
genSMInstruction (SPure (Binaries ops)) = vsep (map genBinOp ops)
genSMInstruction (SPure Discard) = "pop"
genSMInstruction (SPure Halibut) = vsep
  [ "istore 1"
  , "istore 2"
  , "istore 3"
  , "iload 2"
  , "iload 1"
  , "iload 3"
  ]
genSMInstruction (SIO ioInst) = genIOInstruction ioInst
genSMInstruction inst = "; Unsupported SMInstruction: " <> viaShow inst

genBinOp ∷ BinaryOperation → Doc ann
genBinOp Add = "iadd"
genBinOp Sub = "isub"
genBinOp Mul = "imul"
genBinOp Div = "idiv"
genBinOp Mod = "irem"

-- | 2. Generowanie instrukcji Pamięci (LSU / RAM)
genLSInstruction ∷ LSInstruction → Doc ann
genLSInstruction Load = vsep
  [ "; Load (address -> value)"
  , "getstatic Main/ram [I"
  , "swap"
  , "iaload"
  ]
genLSInstruction Store = vsep
  [ "; Store (value, address -> address, value)"
  , "istore 1 ; value"
  , "getstatic Main/ram [I"
  , "swap"
  , "iload 1"
  , "iastore"
  ]
genLSInstruction RStore = vsep
  [ "; RStore (address, value)"
  , "istore 1 ; value"
  , "getstatic Main/ram [I"
  , "swap"
  , "iload 1"
  , "iastore"
  ]
genLSInstruction (LoadD a) = vsep
  [ "getstatic Main/ram [I"
  , "sipush " <> pretty a
  , "iaload"
  ]
genLSInstruction (StoreI v) = vsep
  [ "istore 1 ; address"
  , "getstatic Main/ram [I"
  , "iload 1"
  , "sipush " <> pretty v
  , "iastore"
  ]
genLSInstruction (RStoreD a) = vsep
  [ "istore 1 ; value"
  , "getstatic Main/ram [I"
  , "sipush " <> pretty a
  , "iload 1"
  , "iastore"
  ]
genLSInstruction (MIO ioInst) = genIOInstruction ioInst
genLSInstruction inst = "; Unsupported LSInstruction: " <> viaShow inst

-- | 3. Generowanie instrukcji Sterowania (CPU)
genCFInstruction ∷ CFInstruction → Doc ann
genCFInstruction (Mark l) =
  "label_" <> pretty l <> ":"
genCFInstruction (Labeled Jump (LImmediate l)) =
  "goto label_" <> pretty l
genCFInstruction Return =
  "return"
genCFInstruction (Branch test (BImmediate l)) =
  jasminBranchPred test <+> "label_" <> pretty l
genCFInstruction inst = "; Control Flow: " <> viaShow inst

jasminBranchPred ∷ BranchTest → Doc ann
jasminBranchPred BEZ  = "ifeq"
jasminBranchPred BNZ  = "ifne"
jasminBranchPred BLZ  = "iflt"
jasminBranchPred BGZ  = "ifgt"
jasminBranchPred BLEZ = "ifle"
jasminBranchPred BGEZ = "ifge"

-- | 4. Instrukcje WE/WY (I/O)
genIOInstruction ∷ IOInstruction → Doc ann
genIOInstruction OutputChar = vsep
  [ "getstatic java/lang/System/out Ljava/io/PrintStream;"
  , "swap"
  , "i2c"
  , "invokevirtual java/io/PrintStream/print(C)V"
  ]
genIOInstruction OutputDec = vsep
  [ "getstatic java/lang/System/out Ljava/io/PrintStream;"
  , "swap"
  , "invokevirtual java/io/PrintStream/println(I)V"
  ]
genIOInstruction InputChar = vsep
  [ "getstatic java/lang/System/in Ljava/io/InputStream;"
  , "invokevirtual java/io/InputStream/read()I"
  ]
genIOInstruction _ = "; Unsupported I/O"
