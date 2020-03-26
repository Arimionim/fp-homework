module Task11Test (spec) where

import Test.Hspec
import Task11


spec :: Spec
spec = do
  describe "nextDay" $ do
    it "next of friday" $ do
      sum Friday shouldBe Saturday

    it "nex of Sunday" $ do
      sum Sunday shouldBe Monday
