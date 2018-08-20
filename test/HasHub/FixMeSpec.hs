module HasHub.FixMeSpec where


import Test.Hspec

import System.Directory (emptyPermissions, setPermissions, getPermissions)

import HasHub.FixMe




spec :: Spec
spec = do
  describe "are all in" $ do
    let sut = areAllIn :: [Int] -> [Int] -> Validation [NonExistentError Int] ()

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

  describe "is writable" $ do
    describe "success" $ do
      it "existing path" $ do
        let act = isWritable "test/dummies/output/out.txt"

        act `shouldReturn` Success ()

      it "writable directory" $ do
        let act = isWritable "test/dummies/output/xxx.txt"

        act `shouldReturn` Success ()

    describe "failure" $ do
      it "not writable path" $ do
        let fp = "test/dummies/output/out.txt"
        origin <- getPermissions fp
        setPermissions fp emptyPermissions

        let act = isWritable fp

        act `shouldReturn` Failure [NotWritableFileError fp]

        setPermissions fp origin

      it "not parent directory" $ do
        let fp = "test/dummies/output/xxx.txt"
        let dp = "test/dummies/output"
        origin <- getPermissions dp
        setPermissions dp emptyPermissions

        let act = isWritable fp

        act `shouldReturn` Failure [NotWritableDirectoryError dp]

        setPermissions dp origin

      it "not writable directory" $ do
        let fp = "test/dummies/output/xxx/out.txt"

        let act = isWritable fp

        act `shouldReturn` Failure [NotWritableDirectoryError "test/dummies/output/xxx"]

    describe "message" $ do
      it "not writable file" $ do
        let act = toMessage $ NotWritableFileError "test/dummies/output/out.txt"

        act `shouldBe` "not writable file: test/dummies/output/out.txt"

      it "not writable directory" $ do
        let act = toMessage $ NotWritableDirectoryError "test/dummies/output"

        act `shouldBe` "not writable directory: test/dummies/output"
