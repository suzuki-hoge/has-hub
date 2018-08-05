{-# LANGUAGE OverloadedStrings #-}


module Main where


import Data.Either.Validation (Validation(..))

import HasHub.Client (getClient, Client(..))

import HasHub.Object.Epic as E
import qualified HasHub.Object.Issue.Data as I -- rest
import HasHub.Object.Milestone as M
import HasHub.Object.Label as L
import HasHub.Object.Collaborator as C
import HasHub.Object.Pipeline as P
import HasHub.Object.FixMe


f :: [E.Number] -> [Milestone] -> [Label] -> [Collaborator] -> [Pipeline] -> ([Milestone], [Pipeline])
f es ms ls cs ps = (ms, ps)


printFixMe :: [FixMe] -> IO ()
printFixMe fs = do
  mapM_ print fs


createObjects :: Client -> ([Milestone], [Pipeline]) -> IO ()
createObjects client (ms, ps) = do
  epic <- E.create client (I.Title "API の実装") (I.Body "+ [ ] 実装\n+ [ ] テスト") (Just $ M.Number 1) [Collaborator "suzuki-hoge"] [Label "開発"] [E.Epic (E.Number 3) (E.Title "joining"), E.Epic (E.Number 9) (E.Title "グループ機能")] (Just $ I.Pipeline (I.PipelineId "5b0577fa2133e1068138aabc") (I.PipelineName "進行中")) (Just $ I.Estimate (1 :: Double))

  print epic


run :: String -> String -> IO ()
run gToken zToken = do
  client <- getClient gToken "suzuki-hoge" "has-hub-workspace" zToken

  ve <- E.validate client []
  vm <- M.validate client []
  vl <- L.validate client []
  vc <- C.validate client []
  vp <- P.validate client []
--  ve <- E.validate client [E.Number 9, E.Number 10]
--  vm <- M.validate client [Title "sprint 6", Title "sprint 8"]
--  vl <- L.validate client [Label "bug", Label "setup", Label "dev", Label "実装", Label "ほげら"]
--  vc <- C.validate client [Collaborator "suzuki-john", Collaborator "suzuki-hoge"]
--  vp <- P.validate client [PipelineName "backlog", PipelineName "foo", PipelineName "sprint backlog"]

  let vs = f <$> ve <*> vm <*> vl <*> vc <*> vp

  case vs of
    Success x -> createObjects client x
    Failure x -> printFixMe x

-- todo proxy
-- todo module EpicAndIssue

-- todo log


main :: IO ()
main = undefined
