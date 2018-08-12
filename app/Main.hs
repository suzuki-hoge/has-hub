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


op :: IO ()
op = do
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/objects/success/full_parameter_epic.yaml" >>= print
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/failure/invalid_yaml.yaml" >>= print
  OP.read "/Users/ryo/Dropbox/Developments/haskell/has-hub/test/yaml/failure/xxxxxxxxxxxx.yaml" >>= print


oc :: IO ()
oc = do
  os <- OC.referAll
  print $ [SharpEpicNumber2 "#3", QuestionEpicNumber2 "?99"] `OV.areAllIn` os
  print $ [SharpEpicNumber2 "#1"] `OV.areAllIn` os


mc :: IO ()
mc = do
  ms <- MC.referAll
  print $ [MilestoneTitle2 "sprint 1"] `MV.areAllIn` ms
  print $ [MilestoneTitle2 "unknown"] `MV.areAllIn` ms


lc :: IO ()
lc = do
  ls <- LC.referAll
  print $ [Label2 "setup"] `LV.areAllIn` ls
  print $ [Label2 "unknown"] `LV.areAllIn` ls


cc :: IO ()
cc = do
  cs <- CC.referAll
  print $ [Collaborator2 "suzuki-hoge"] `CV.areAllIn` cs
  print $ [Collaborator2 "unknown"] `CV.areAllIn` cs


pc :: IO ()
pc = do
  ps <- PC.referAll
  print $ [PipelineName2 "sprint backlog"] `PV.areAllIn` ps
  print $ [PipelineName2 "unknown"] `PV.areAllIn` ps


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


main = do
  op

  set

  oc
  mc
  lc
  cc
  pc

  createObjects
