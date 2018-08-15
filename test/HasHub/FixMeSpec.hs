module HasHub.FixMeSpec where


import Test.Hspec

import HasHub.FixMe


sut :: [Int] -> [Int] -> Validation [NonExistentError Int] ()
sut = areAllIn


spec :: Spec
spec = do
  describe "are all in" $ do
    describe "success" $ do
      it "double in a few" $ do
        let act = [1, 2] `sut` [1, 2]

        act `shouldBe` Success ()

      it "single in a few" $ do
        let act = [1] `sut` [1, 2]

        act `shouldBe` Success ()

      it "single in single" $ do
        let act = [1] `sut` [1]

        act `shouldBe` Success ()

      it "empty in single" $ do
        let act = [] `sut` [1]

        act `shouldBe` Success ()

      it "empty in empty" $ do
        let act = [] `sut` []

        act `shouldBe` Success ()

    describe "failure" $ do
      it "single in empty" $ do
        let act = [1] `sut` []

        act `shouldBe` Failure [NonExistentError 1]

      it "double in empty" $ do
        let act = [1, 2] `sut` []

        act `shouldBe` Failure [NonExistentError 1, NonExistentError 2]

      it "single in single" $ do
        let act = [1] `sut` [2]

        act `shouldBe` Failure [NonExistentError 1]
