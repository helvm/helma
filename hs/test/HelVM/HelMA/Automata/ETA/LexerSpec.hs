module HelVM.HelMA.Automata.ETA.LexerSpec (spec) where

import           HelVM.HelMA.Automata.ETA.Lexer

import           HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import           HelVM.HelMA.Automata.ETA.FileUtil

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                 (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "original" $
    forM_ [ ("hello"   , helloTL  )
          , ("hello2"  , hello2TL )
          , ("pip"     , pipTL    )
          , ("pip2"    , pip2TL   )
          , ("fact"    , factTL   )
          , ("bottles" , bottlesTL)
          , ("crlf"    , crlfTL   )
          ] $ \(fileName , tl) ->
      describe fileName $ do
        let path = "original" </> fileName
        it ("minified" </> path) $
          (show . readTokens <$> readEtaFile path) `goldenShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
        it ("tokenize" </> path) $
          tokenize           <$> readEtaFile path  `shouldReturn` tl

  describe "from-eas" $
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
          ] $ \(fileName , tl) ->
      describe fileName $ do
        let path = "from-eas" </> fileName
        it ("minified" </> path) $
          (show . readTokens <$> readEtaFile path) `goldenShouldIO` buildAbsoluteEtaFileName ("minified" </> path)
        it ("tokenize" </> fileName) $
          tokenize           <$> readEtaFile path  `shouldReturn` tl
