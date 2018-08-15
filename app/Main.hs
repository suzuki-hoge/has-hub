module Main where


import Data.Either.Validation (Validation(..))

import HasHub.Connection.Connector (set) -- todo

import HasHub.Object.Object.Parser as OP

import HasHub.Object.Object.Client as OC
import HasHub.Object.Milestone.Client as MC
import HasHub.Object.Label.Client as LC
import HasHub.Object.Collaborator.Client as CC
import HasHub.Object.Pipeline.Client as PC

import qualified HasHub.Object.Object.Validator as OV
import qualified HasHub.Object.Milestone.Validator as MV
import qualified HasHub.Object.Label.Validator as LV
import qualified HasHub.Object.Collaborator.Validator as CV
import qualified HasHub.Object.Pipeline.Validator as PV

import HasHub.FixMe


setup :: String -> String -> IO ()
setup g z = do
  set g z


oc :: IO ()
oc = do
  os <- OC.referAll
  print os
  printVs $ [SharpEpicNumber "#58", QuestionEpicNumber "?99"] `OV.areAllIn` os
  printVs $ [SharpEpicNumber "#1", SharpEpicNumber "#50"] `OV.areAllIn` os
  printVs $ OV.noDuplication [EpicLinkNumber "?2", EpicLinkNumber "?1", EpicLinkNumber "?3", EpicLinkNumber "?1", EpicLinkNumber "?2"]
  printVs $ OV.linkNumberFormat [EpicLinkNumber "?1", EpicLinkNumber "1", EpicLinkNumber "#1", EpicLinkNumber "?", EpicLinkNumber "? 1", EpicLinkNumber "??1"]
  printVs $ OV.parentNumberFormat [SharpEpicNumber "#1", SharpEpicNumber "#", SharpEpicNumber "# 1", QuestionEpicNumber "1", QuestionEpicNumber "?#"]
  printVs $ OV.linking [(2, EpicLinkNumber "?1")] [(1, QuestionEpicNumber "?1"), (2, QuestionEpicNumber "?1"), (2, SharpEpicNumber "#1"), (2, QuestionEpicNumber "?2"), (3, QuestionEpicNumber "?2")]


mc :: IO ()
mc = do
  ms <- MC.referAll
  print ms
  printVs $ [MilestoneTitle "sprint 1"] `MV.areAllIn` ms
  printVs $ [MilestoneTitle "unknown"] `MV.areAllIn` ms



lc :: IO ()
lc = do
  ls <- LC.referAll
  print ls
  printVs $ [Label "setup"] `LV.areAllIn` ls
  printVs $ [Label "unknown1", Label "unknown2"] `LV.areAllIn` ls


cc :: IO ()
cc = do
  cs <- CC.referAll
  print cs
  printVs $ [Collaborator "suzuki-hoge"] `CV.areAllIn` cs
  printVs $ [Collaborator "unknown"] `CV.areAllIn` cs


pc :: IO ()
pc = do
  ps <- PC.referAll
  print ps
  printVs $ [PipelineName "sprint backlog"] `PV.areAllIn` ps
  printVs $ [PipelineName "unknown"] `PV.areAllIn` ps


createObjects :: IO ()
createObjects = do
  parsed <- OP.readObjects "test/yaml/objects/epic_and_issue.yaml"
  case parsed of
    Success(objs) -> do
      let obj = objs !! 0
      x <- create obj [] [] []
      print x
    Failure(errs) -> do
      print errs


printVs :: (FixMe fm) => Validation [fm] () -> IO ()
printVs (Success _)   = putStrLn "[]"
printVs (Failure fms) = mapM_ (putStrLn . toMessage) fms


main = undefined
