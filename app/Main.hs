{-# LANGUAGE OverloadedStrings #-}


module Main where


import HasHub.Connection.Connector (set) -- todo

import HasHub.Object.Milestone.Client as MC
import HasHub.Object.Label.Client as LC
import HasHub.Object.Collaborator.Client as CC
import HasHub.Object.Pipeline.Client as PC
import qualified HasHub.Object.Milestone.Validator as MV
import qualified HasHub.Object.Label.Validator as LV
import qualified HasHub.Object.Collaborator.Validator as CV
import qualified HasHub.Object.Pipeline.Validator as PV


ma :: IO ()
ma = do
  ms <- map toMilestoneTitle <$> MC.referAll
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
  ps <- map toPipelineName <$> PC.referAll
  print $ [PipelineName2 "sprint backlog"] `PV.areAllIn` ps
  print $ [PipelineName2 "unknown"] `PV.areAllIn` ps


main = do
  set
  ma
  la
  ca
  pa
