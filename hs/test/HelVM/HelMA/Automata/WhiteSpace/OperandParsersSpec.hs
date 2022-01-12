module HelVM.HelMA.Automata.WhiteSpace.OperandParsersSpec (spec) where

import           HelVM.HelMA.Automata.WhiteSpace.OperandParsers
import           HelVM.HelMA.Automata.WhiteSpace.Token

import           HelVM.Expectations

import           Test.Hspec                                     (Spec, describe, it)

spec :: Spec
spec =
  describe "parse" $
    forM_ [ ([N]             , (0 ,  0 , ""   ))
          , ([S , N]         , (0 ,  0 , "0"  ))
          , ([T , N]         , (1 ,  0 , "1"  ))
          , ([S , S , N]     , (0 ,  0 , "00" ))
          , ([S , T , N]     , (1 ,  1 , "01" ))
          , ([T , S , N]     , (2 ,  0 , "10" ))
          , ([T , T , N]     , (3 , -1 , "11" ))
          , ([S , S , S , N] , (0 ,  0 , "000"))
          , ([S , S , T , N] , (1 ,  1 , "001"))
          , ([S , T , S , N] , (2 ,  2 , "010"))
          , ([S , T , T , N] , (3 ,  3 , "011"))
          , ([T , S , S , N] , (4 ,  0 , "100"))
          , ([T , S , T , N] , (5 , -1 , "101"))
          , ([T , T , S , N] , (6 , -2 , "110"))
          , ([T , T , T , N] , (7 , -3 , "111"))
          ] $ \(input , (n , i , d)) ->
      describe (show input) $ do
        it "natural" $ (fst <$> parseNatural     input) `shouldSafe` n
        it "integer" $ (fst <$> parseInteger     input) `shouldSafe` i
        it "digits"  $ (fst <$> parseDigitString input) `shouldSafe` d
