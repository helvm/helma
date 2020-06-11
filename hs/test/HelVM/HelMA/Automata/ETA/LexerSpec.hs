module HelVM.HelMA.Automata.ETA.LexerSpec (spec) where

import HelVM.HelMA.Automata.ETA.Lexer

import HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import HelVM.HelMA.Automata.ETA.FileUtil

import HelVM.HelMA.Automata.Expectations

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "original" $ do
    forM_ [ ("hello"   , helloTL  )
          , ("hello2"  , hello2TL )
          , ("pip"     , pipTL    )
          , ("pip2"    , pip2TL   )
          , ("fact"    , factTL   )
          , ("bottles" , bottlesTL)
          , ("crlf"    , crlfTL   )
          ] $ \(fileName , tl) -> do
      describe fileName $ do
        it ("minified" </> fileName) $ do
          (show . readTokens <$> readEtaFile ("original" </> fileName)) `goldenShouldReturn` buildAbsoluteEtaFileName ("original" </> "minified" </> fileName)
        it ("tokenize" </> fileName) $ do
          tokenize           <$> readEtaFile ("original" </> fileName)  `shouldReturn` tl

  describe "from-eas" $ do
    forM_ [ ("true"     , trueEASTL    )
          , ("hello"    , helloEASTL   )
          , ("pip"      , pipEASTL     )
          , ("pip2"     , pip2EASTL    )
          , ("reverse"  , reverseEASTL )
          , ("function" , functionEASTL)
          , ("writestr" , writeStrEASTL)
          , ("hello2"   , hello2EASTL  )
          , ("hello3"   , hello3EASTL  )
          , ("hello4"   , hello3EASTL  )
          , ("writenum" , writeNumEASTL)
          , ("multiply" , multiplyEASTL)
          , ("readnum"  , readNumEASTL )
          , ("fact"     , factEASTL    )
          , ("bottles"  , bottlesEASTL )
          , ("euclid"   , euclidEASTL  )
          ] $ \(fileName , tl) -> do
      describe fileName $ do
        it ("minified" </> fileName) $ do
          (show . readTokens <$> readEtaFile ("from-eas" </> fileName)) `goldenShouldReturn` buildAbsoluteEtaFileName ("from-eas" </> "minified" </> fileName)
        it ("tokenize" </> fileName) $ do
          tokenize           <$> readEtaFile ("from-eas" </> fileName)  `shouldReturn` tl
