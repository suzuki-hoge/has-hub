{-# LANGUAGE OverloadedStrings #-}


module Main where


import Data.Either.Validation (Validation(..))

import HasHub.Client (getClient, Client(..))

import HasHub.Object.Object (createEpic)

import HasHub.Object.Milestone as M
import HasHub.Object.Label as L
import HasHub.Object.Collaborator as C
import HasHub.Object.Pipeline as P
import HasHub.Object.Object as O
import HasHub.Object.FixMe

import HasHub.Yaml.Parser


--f :: [E.Number] -> [Milestone] -> [Label] -> [Collaborator] -> [Pipeline] -> ([Milestone], [Pipeline])
--f es ms ls cs ps = (ms, ps)
--
--
--printFixMe :: [FixMe] -> IO ()
--printFixMe fs = do
--  mapM_ print fs
--
--
--createObjects :: Client -> ([Milestone], [Pipeline]) -> IO ()
--createObjects client (ms, ps) = do
--  -- epic <- E.create client (I.Title "API の実装") (I.Body "+ [ ] 実装\n+ [ ] テスト") (Just $ M.MilestoneNumber 1) [Collaborator "suzuki-hoge"] [Label "開発"] [E.Epic (E.Number 3) (E.Title "joining"), E.Epic (E.Number 9) (E.Title "グループ機能")] (Just $ Pipeline (PipelineId "5b0577fa2133e1068138aabc") ("進行中")) (Just $ I.Estimate (1 :: Double))
--
--  print ()
--
--
run :: String -> String -> IO ()
run gToken zToken = do
  vs <- parseObjects "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/objects/success/full_parameter_epic.yaml"


  case vs of
    Success obs -> do
      client <- getClient gToken "suzuki-hoge" "has-hub-workspace" zToken "has-hub.log"

      print =<< O.validateAllExists client [SharpEpicNumber "#1"]

      print $ O.validateNoDuplicate [EpicLinkNumber "#1", EpicLinkNumber "#1"]

      print $ O.validateLinking [(1, EpicLinkNumber "?1")] [(2, QuestionEpicNumber "?1")]
      print $ O.validateLinking [(1, EpicLinkNumber "?1")] [(2, QuestionEpicNumber "?2")]
      print $ O.validateLinking [(5, EpicLinkNumber "?1")] [(2, QuestionEpicNumber "?1")]

      print $ O.epicLinkNumberFormat [EpicLinkNumber "?1"]
      print $ O.parentEpicNumberFormat [SharpEpicNumber "#1", QuestionEpicNumber "?1"]

--      let vo = O.validateNoDuplicate [QuestionEpicNumber 3, QuestionEpicNumber 1, QuestionEpicNumber 2, QuestionEpicNumber 1]

--      let ms = [Milestone (MilestoneNumber 1) (MilestoneTitle "sprint 1") (Just $ StartOn "2018-04-01T00:00:00Z") (Just $ DueOn "2018-04-30T23:59:59Z")]
--      let ps = [Pipeline "5b0577fa2133e1068138aabc" "sprint backlog"]
--      let links = [LinkedEpic (QuestionEpicNumber 1) (EpicNumber 51)]
--
--      paireds <- mapM (createEpic client ms ps links) obs
--
--      print paireds

    Failure x -> do
      print x


--  ve <- E.validate client []
--  vm <- M.validate client []
--  vl <- L.validate client []
--  vc <- C.validate client []
--  vp <- P.validate client []
--  ve <- E.validate client [E.Number 9, E.Number 10]
--  vm <- M.validate client [M.Title "sprint 6", M.Title "sprint 8"]
--  vl <- L.validate client [Label "bug", Label "setup", Label "dev", Label "実装", Label "ほげら"]
--  vc <- C.validate client [Collaborator "suzuki-john", Collaborator "suzuki-hoge"]
--  vp <- P.validate client [PipelineName "backlog", PipelineName "foo", PipelineName "sprint backlog"]
--
--  let vs = f <$> ve <*> vm <*> vl <*> vc <*> vp
--
--  case vs of
--    Success x -> createObjects client x
--    Failure x -> printFixMe x

  print "run"


main :: IO ()
main = undefined


-- todo next step is spec migration