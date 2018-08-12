{-# LANGUAGE OverloadedStrings #-}


module Main where


import HasHub.Connection.Connector (set) -- todo

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


oa :: IO ()
oa = do
  os <- OC.referAll
  print $ [SharpEpicNumber2 "#3", QuestionEpicNumber2 "?99"] `OV.areAllIn` os
  print $ [SharpEpicNumber2 "#1"] `OV.areAllIn` os


ma :: IO ()
ma = do
  ms <- MC.referAll
  print $ [MilestoneTitle2 "sprint 1"] `MV.areAllIn` ms
  print $ [MilestoneTitle2 "unknown"] `MV.areAllIn` ms


la :: IO ()
la = do
  ls <- LC.referAll
  print $ [Label2 "setup"] `LV.areAllIn` ls
  print $ [Label2 "unknown"] `LV.areAllIn` ls


ca :: IO ()
ca = do
  cs <- CC.referAll
  print $ [Collaborator2 "suzuki-hoge"] `CV.areAllIn` cs
  print $ [Collaborator2 "unknown"] `CV.areAllIn` cs


pa :: IO ()
pa = do
  ps <- PC.referAll
  print $ [PipelineName2 "sprint backlog"] `PV.areAllIn` ps
  print $ [PipelineName2 "unknown"] `PV.areAllIn` ps


main = do
  set
  oa
  ma
  la
  ca
  pa
