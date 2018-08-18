module HasHub.Command.EnvSpec where


import Test.Hspec

import System.Environment (setEnv)

import HasHub.Command.Env

import HasHub.FixMe (FixMe(..), Validation(..))


spec :: Spec
spec = do
  describe "owner" $ do
    describe "success" $ do
      it "detect https protocol" $ do
        let act = fixOwner Nothing "test/dummies/cwd/valid_https_protocol_config"

        act `shouldReturn` Success "https-owner"

      it "detect git protocol" $ do
        let act = fixOwner Nothing "test/dummies/cwd/valid_git_protocol_config"

        act `shouldReturn` Success "git-owner"

      it "detect git protocol with dot" $ do
        let act = fixOwner Nothing "test/dummies/cwd/valid_git_protocol_with_dot_config"

        act `shouldReturn` Success "git-owner"

      it "specified" $ do
        let act = fixOwner (Just "suzuki-hoge") "test/dummies/cwd/xxx"

        act `shouldReturn` Success "suzuki-hoge"

    describe "failure" $ do
      it "not git directory" $ do
        let act = fixOwner Nothing "test/dummies/cwd/xxx"

        act `shouldReturn` Failure [ConfigurationError "test/dummies/cwd/xxx is not found."]

      it "no remote config" $ do
        let act = fixOwner Nothing "test/dummies/cwd/no_remote_config"

        act `shouldReturn` Failure [ConfigurationError "remote config missing."]

      it "invalid protocol" $ do
        let act = fixOwner Nothing "test/dummies/cwd/invalid_remote_config"

        act `shouldReturn` Failure [ConfigurationError "invalid remote config."]

  describe "repository" $ do
    describe "success" $ do
      it "detect https protocol" $ do
        let act = fixRepository Nothing "test/dummies/cwd/valid_https_protocol_config"

        act `shouldReturn` Success "https-repository"

      it "detect git protocol" $ do
        let act = fixRepository Nothing "test/dummies/cwd/valid_git_protocol_config"

        act `shouldReturn` Success "git-repository"

      it "detect git protocol with dot" $ do
        let act = fixRepository Nothing "test/dummies/cwd/valid_git_protocol_with_dot_config"

        act `shouldReturn` Success "git-repository"

      it "specified" $ do
        let act = fixRepository (Just "has-hub-workspace") "test/dummies/cwd/not_git_directory"

        act `shouldReturn` Success "has-hub-workspace"

    describe "failure" $ do
      it "not git directory" $ do
        let act = fixRepository Nothing "test/dummies/cwd/xxx"

        act `shouldReturn` Failure [ConfigurationError "test/dummies/cwd/xxx is not found."]

      it "no remote config" $ do
        let act = fixRepository Nothing "test/dummies/cwd/no_remote_config"

        act `shouldReturn` Failure [ConfigurationError "remote config missing."]

      it "invalid protocol" $ do
        let act = fixRepository Nothing "test/dummies/cwd/invalid_remote_config"

        act `shouldReturn` Failure [ConfigurationError "invalid remote config."]

  describe "git hub token" $ do
    describe "success" $ do
      it "detect" $ do
        let act = fixToken Nothing "git-hub-token:" "test/dummies/home/valid_config"

        act `shouldReturn` Success "12345678"

      it "specified" $ do
        let act = fixToken (Just "xxxxxxxx") "git-hub-token:" "test/dummies/home/xxx"

        act `shouldReturn` Success "xxxxxxxx"

    describe "failure" $ do
      it "no config" $ do
        let act = fixToken Nothing "git-hub-token:" "test/dummies/home/xxx"

        act `shouldReturn` Failure [ConfigurationError "test/dummies/home/xxx is not found."]

      it "invalid config" $ do
        let act = fixToken Nothing "git-hub-token:" "test/dummies/home/invalid_config"

        act `shouldReturn` Failure [ConfigurationError "git-hub-token config missing."]

  describe "zen hub token" $ do
    describe "success" $ do
      it "detect" $ do
        let act = fixToken Nothing "zen-hub-token:" "test/dummies/home/valid_config"

        act `shouldReturn` Success "12345678abcdefgh"

      it "specified" $ do
        let act = fixToken (Just "xxxxxxxxxxxxxxxx") "zen-hub-token:" "test/dummies/home/xxx"

        act `shouldReturn` Success "xxxxxxxxxxxxxxxx"

    describe "failure" $ do
      it "no config" $ do
        let act = fixToken Nothing "zen-hub-token:" "test/dummies/home/xxx"

        act `shouldReturn` Failure [ConfigurationError "test/dummies/home/xxx is not found."]

      it "invalid config" $ do
        let act = fixToken Nothing "zen-hub-token:" "test/dummies/home/invalid_config"

        act `shouldReturn` Failure [ConfigurationError "zen-hub-token config missing."]

  describe "log path" $ do
      it "default" $ do
        let act = fixLogPath Nothing

        act `shouldBe` "~/has-hub.log"

      it "specified" $ do
        let act = fixLogPath (Just "./out")

        act `shouldBe` "./out"

  describe "proxy" $ do
      it "no env" $ do
        let act = fixProxy Nothing

        act `shouldReturn` Nothing

      it "from env" $ do
        setEnv "https_proxy" "xxx.xxx.xxx.xxx:xxxx"

        let act = fixProxy Nothing

        act `shouldReturn` (Just "xxx.xxx.xxx.xxx:xxxx")

      it "specified" $ do
        let act = fixProxy (Just "xxx.xxx.xxx.xxx:xxxx")

        act `shouldReturn` (Just "xxx.xxx.xxx.xxx:xxxx")

  describe "message" $ do
    it "message" $ do
      let act = toMessage $ ConfigurationError "missing."

      act `shouldBe` "configuration error: missing."
