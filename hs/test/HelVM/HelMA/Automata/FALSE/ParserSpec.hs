module HelVM.HelMA.Automata.FALSE.ParserSpec (spec) where

import           HelVM.HelMA.Automata.FALSE.FileExtra
import           HelVM.HelMA.Automata.FALSE.Parser

import           HelVM.HelIO.Control.Safe

import           HelVM.GoldenExpectations

import           Test.Hspec                           (Spec, describe, it)

spec :: Spec
spec = do
  describe "parser" $
    forM_ [
        "helma/add"
      , "helma/comment"
      , "helma/emptyString"

      , "helma/copy"
      , "helma/factorial"
      , "helma/primeNumber"

      , "strlen/inline/copy"
      , "strlen/inline/factorial"
      , "strlen/inline/primeNumber"

      , "strlen/examples/addcr"
--      , "strlen/examples/alloc"
      , "strlen/examples/arg"
      , "strlen/examples/argtest"
--      , "strlen/examples/bin2f"
--      , "strlen/examples/copy"
--      , "strlen/examples/fac"
      , "strlen/examples/filter"
      , "strlen/examples/helloworld"
      , "strlen/examples/lambda"
      , "strlen/examples/prime"
      , "strlen/examples/strip"
--      , "strlen/examples/tafels"
--      , "strlen/examples/vcheck"

      , "strlen/contrib/Ben_Schaeffer/detab"
      , "strlen/contrib/Ben_Schaeffer/postfix_while"

      , "strlen/contrib/Eelko_de_Vos/chkbrack"
      , "strlen/contrib/Eelko_de_Vos/countwl"
      , "strlen/contrib/Eelko_de_Vos/crunch"
      , "strlen/contrib/Eelko_de_Vos/decrunch"
      , "strlen/contrib/Eelko_de_Vos/depack"
      , "strlen/contrib/Eelko_de_Vos/eval"
--      , "strlen/contrib/Eelko_de_Vos/modseek" --FIXME TLDR
      , "strlen/contrib/Eelko_de_Vos/pack"
      , "strlen/contrib/Eelko_de_Vos/readable"
      , "strlen/contrib/Eelko_de_Vos/remcom"
      , "strlen/contrib/Eelko_de_Vos/split"

      , "strlen/contrib/Herb_Wollman/ASCII"
      , "strlen/contrib/Herb_Wollman/Fibonacci"

      , "strlen/contrib/Marcel_van_Kervinck/queens"
--      , "strlen/contrib/Marcel_van_Kervinck/tic" --FIXME TLDR 4s

      , "strlen/contrib/Peter_Bengtsson/DoASC"
      , "strlen/contrib/Peter_Bengtsson/GetASC"

      , "strlen/contrib/Steinar_Knutsen/htmlcol"
      , "strlen/contrib/Steinar_Knutsen/passwdc"

      , "strlen/contrib/Thomas_Fischbacher/life"

      , "strlen/contrib/self"

      , "other/benHoyt/bin2f"
      , "other/benHoyt/dump"
      , "other/benHoyt/eval"
      , "other/benHoyt/fac"
--      , "other/benHoyt/fbreak"

      , "other/iliiliiliili/read"
      , "other/iliiliiliili/sum"

      , "other/morphett/factorial"
      , "other/morphett/factorial2"
      , "other/morphett/reverse"

      , "other/thunderseethe/prog"

      ] $ \ path ->
      it path $
        safeIOToPTextIO (parse <$> readFFile path) `goldenShouldIO` buildAbsoluteFIlFileName path
