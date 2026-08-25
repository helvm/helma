module HelVM.HelMA.Automata.Piet.LLVM.Internal.LLVM
  ( resolver
  , withHostDynamicJITTargetMachine
  , withLinkedModule
  ) where

import           HelVM.HelMA.Automata.Piet.LLVM.CompileOption
import qualified HelVM.HelMA.Automata.Piet.LLVM.Internal.Runtime as P
import qualified LLVM.AST                                        as AST
import qualified LLVM.CodeGenOpt                                 as CodeGenOpt
import qualified LLVM.CodeModel                                  as CodeModel
import           LLVM.Context
import           LLVM.Linking
import           LLVM.Module
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.PassManager
import qualified LLVM.Relocation                                 as Reloc
import           LLVM.Target

withLinkedModule ∷ OptimizationLevel → AST.Module → (Module → IO a) → IO a
withLinkedModule optimizationLevel ast f =
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx P.runtimeAssembly $ \runtimeMod ->
      withModuleFromAST ctx ast $ \astMod ->
        withPassManager (passSetSpec optimizationLevel) $ \passManager -> do
          linkModules astMod runtimeMod
          let linkedModule = astMod
          _ <- runPassManager passManager linkedModule
          f linkedModule

resolver ∷ IRCompileLayer l → SymbolResolver
resolver cl = SymbolResolver dylibResolver' externalResolver' where
  dylibResolver' sym = findSymbol cl sym True
  externalResolver' sym = do
    addr <- getSymbolAddressInProcess sym
    return $ return $ JITSymbol addr defaultJITSymbolFlags

withHostDynamicJITTargetMachine ∷ (TargetMachine → IO a) → IO a
withHostDynamicJITTargetMachine f = do
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.Default CodeModel.JITDefault CodeGenOpt.Default f

passSetSpec ∷ OptimizationLevel → PassSetSpec
passSetSpec NoOptimization          = defaultCuratedPassSetSpec
passSetSpec OptimizationLevelLow    = defaultCuratedPassSetSpec { optLevel = Just 1 }
passSetSpec OptimizationLevelMiddle = defaultCuratedPassSetSpec { optLevel = Just 2 }
passSetSpec OptimizationLevelHigh   = defaultCuratedPassSetSpec { optLevel = Just 3 }
passSetSpec SizeLevelLow            = defaultCuratedPassSetSpec { sizeLevel = Just 1 }
passSetSpec SizeLevelHigh           = defaultCuratedPassSetSpec { sizeLevel = Just 2 }
