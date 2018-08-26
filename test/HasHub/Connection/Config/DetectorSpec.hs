module HasHub.Connection.Config.DetectorSpec where


import Test.Hspec

import System.Environment (unsetEnv, setEnv)
import System.Directory (getHomeDirectory)

import HasHub.Connection.Config.Detector

import HasHub.FixMe (FixMe(..), Validation(..))


spec :: Spec
spec = do
  describe "fix config" $ do
    it "no command line arguments and execute at project1" $ do
      let sut = fixConfig Nothing "test/dummies/config/home/project1"

      sut "owner:" `shouldReturn` Success "owner-1"
      sut "repository:" `shouldReturn` Success "workspace-1"
      sut "git-hub-token:" `shouldReturn` Success "12345678"
      sut "zen-hub-token:" `shouldReturn` Success "12345678abcdefgh"

    it "no command line arguments and execute at project2" $ do
      let sut = fixConfig Nothing "test/dummies/config/home/project2"

      sut "owner:" `shouldReturn` Success "owner-2"
      sut "repository:" `shouldReturn` Success "workspace-2"
      sut "git-hub-token:" `shouldReturn` Success "12345678"
      sut "zen-hub-token:" `shouldReturn` Success "12345678abcdefgh"

    it "owner specified by command line argument and execute at project1" $ do
      let sut = fixConfig (Just "suzuki-hoge") "test/dummies/config/home/project1"

      sut "owner:" `shouldReturn` Success "suzuki-hoge"

    it "log-path not specified by command line argument" $ do
      let sut = fixLogPath Nothing "test/dummies/config/home"

      sut "log-full-path:" `shouldReturn` Success "/tmp/has-hub.log"

    it "log-path specified by command line argument" $ do
      let sut = fixLogPath (Just "~/has-hub.log") "test/dummies/config/home"
      home <- getHomeDirectory

      sut "log-full-path:" `shouldReturn` Success (home ++ "/has-hub.log")

    it "not specified by command line argument and no config" $ do
      let sut = fixConfig Nothing "test/dummies/config/home/project1"

      sut "foo-key:" `shouldReturn` Failure [ConfigurationError "foo-key not found in config"]

  describe "detect proxy" $ do
      it "no env" $ do
        unsetEnv "https_proxy"

        let act = detectProxy

        act `shouldReturn` Nothing

      it "from env that lower case name" $ do
        setEnv "https_proxy" "xxx.xxx.xxx.xxx:xxxx"

        let act = detectProxy

        act `shouldReturn` Just "xxx.xxx.xxx.xxx:xxxx"

      it "from env that upper case name" $ do
        setEnv "HTTPS_PROXY" "xxx.xxx.xxx.xxx:xxxx"

        let act = detectProxy

        act `shouldReturn` Just "xxx.xxx.xxx.xxx:xxxx"

  describe "message" $ do
    it "message" $ do
      let act = toMessage $ ConfigurationError "foo-key not found in config"

      act `shouldBe` "configuration error: foo-key not found in config"
