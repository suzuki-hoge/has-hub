{-# LANGUAGE OverloadedStrings #-}


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


setup :: String -> String -> IO ()
setup g z = do
  set g z


op :: IO ()
op = do
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/objects/success/full_parameter_epic.yaml" >>= print
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/failure/invalid_yaml.yaml" >>= print
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/failure/xxxxxxxxxxxx.yaml" >>= print


oc :: IO ()
oc = do
  os <- OC.referAll
  print $ [SharpEpicNumber "#3", QuestionEpicNumber "?99"] `OV.areAllIn` os
  print $ [SharpEpicNumber "#1"] `OV.areAllIn` os


mc :: IO ()
mc = do
  ms <- MC.referAll
  print $ [MilestoneTitle "sprint 1"] `MV.areAllIn` ms
  print $ [MilestoneTitle "unknown"] `MV.areAllIn` ms


lc :: IO ()
lc = do
  ls <- LC.referAll
  print $ [Label "setup"] `LV.areAllIn` ls
  print $ [Label "unknown"] `LV.areAllIn` ls


cc :: IO ()
cc = do
  cs <- CC.referAll
  print $ [Collaborator "suzuki-hoge"] `CV.areAllIn` cs
  print $ [Collaborator "unknown"] `CV.areAllIn` cs


pc :: IO ()
pc = do
  ps <- PC.referAll
  print $ [PipelineName "sprint backlog"] `PV.areAllIn` ps
  print $ [PipelineName "unknown"] `PV.areAllIn` ps


createObjects :: IO ()
createObjects = do
  parsed <- OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/objects/success/full_parameter_epic.yaml"
  case parsed of
    Success(objs) -> do
      let obj = objs !! 0
      x <- create obj [] [] []
      print x
    Failure(errs) -> do
      print "parse error"


main = undefined
