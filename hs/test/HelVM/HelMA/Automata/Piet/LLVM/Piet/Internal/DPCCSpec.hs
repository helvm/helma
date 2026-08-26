module HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.DPCCSpec
  ( main
  , spec
  ) where

import qualified Data.Map                                          as M
import           HelVM.HelMA.Automata.Piet.LLVM.Piet.Internal.DPCC
import           HelVM.HelMA.Automata.Piet.LLVM.SyntaxTestHelper
import           Test.Hspec

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec = do
  describe "dpccsToBackwardDPCCTable" $ do
    forM_
      [ ([], M.empty)
      , ([rl], M.fromList [(rl, [rl, rr, dl, dr, ll, lr, ul, ur])])
      , ([rl, rr, dl, dr, ll, lr, ul, ur], M.fromList [(rl, [rl]), (rr, [rr]), (dl, [dl]), (dr, [dr]), (ll, [ll]), (lr, [lr]), (ul, [ul]), (ur, [ur])])
      , ([rl, dl, ll, ul], M.fromList [(rl, [rl, rr]), (dl, [dl, dr]), (ll, [ll, lr]), (ul, [ul, ur])])
      , ([rr, dr, lr, ur], M.fromList [(rr, [rl, rr]), (dr, [dl, dr]), (lr, [ll, lr]), (ur, [ul, ur])])
      , ([rl, rr], M.fromList [(rl, [rl, dr, ll, ur]), (rr, [rr, dl, lr, ul])])
      , ([rl, ul], M.fromList [(rl, [rl, rr]), (ul, [dl, dr, ll, lr, ul, ur])])
      , ([rr, ul], M.fromList [(rr, [rl, rr]), (ul, [dl, dr, ll, lr, ul, ur])])
      , ([rl, dl], M.fromList [(rl, [rl, rr, ll, lr, ul, ur]), (dl, [dl, dr])])
      , ([rr, dl], M.fromList [(rr, [rl, rr, ll, lr, ul, ur]), (dl, [dl, dr])])
      , ([ur], M.fromList [(ur, [rl, rr, dl, dr, ll, lr, ul, ur])])
      , ([ul, ur], M.fromList [(ul, [rr, dl, lr, ul]), (ur, [rl, dr, ll, ur])])
      ] $ \(dpccs, expected) ->
        context ("when given " ++ show dpccs) $ do
          it "returns a backward DPCC table" $ dpccsToBackwardDPCCTable dpccs `shouldBe` expected
