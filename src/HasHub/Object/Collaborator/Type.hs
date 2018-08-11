{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.Type where


import Data.Aeson (FromJSON(..), Value(Object), (.:))


newtype Collaborator2 = Collaborator2 String deriving (Eq, Show)
instance FromJSON Collaborator2 where
  parseJSON (Object v) = Collaborator2 <$> (v .: "login")

