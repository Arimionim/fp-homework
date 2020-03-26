module Task12Test (spec) where

import Test.Hspec

spec :: Spec


spec :: Spec
spec = do
  describe "sum" $ do
    it "sum 2 3" $ do
        sum (S (S Z)) (S (S (S Z))) shouldBe S (S (S (S (S Z))))
    it "sum 1 0" $ do
        sum (S Z) Z shouldBe S Z

    it "mul 1 2" $ do
        mul (S Z) (S (S Z)) shouldBe (S (S Z))

    it "sub 1 2" $ do
        sub (S Z) (S (S Z)) shouldBe Z
    it "sub 2 1" $ do
        sub (S (S Z)) (S Z) shouldBe (S Z)

    it "intToNat 3" $ do
        intToNat 3 shouldBe S (S (S Z))

    it "natToInt 3" $ do
        natToInt S (S (S Z)) shouldBe 3

    it "compare 1 0" $ do
        compare (S Z) Z shouldBe 1
    it "compare 1 1" $ do
        compare (S Z) (S Z) shouldBe 0

    it "checkEven 1" $ do
        checkEven (S Z) shouldBe True
    it "checkEven 0" $ do
        checkEven Z shouldBe False


    it "divNat 3 2" $ do
        divNat (S (S (S Z))) (S (S Z)) shouldBe S Z

    it "modNat 3 2" $ do
        modNat (S (S (S Z))) (S (S Z)) shouldBe S Z