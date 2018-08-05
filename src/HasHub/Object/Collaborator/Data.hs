{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.Data where


import Data.Aeson (FromJSON(..), Value(Object), (.:))


newtype Collaborator = Collaborator String deriving (Eq)
instance FromJSON Collaborator where
  parseJSON (Object v) = Collaborator <$> (v .: "login")
instance Show Collaborator where
  show (Collaborator name) = "Collaborator(" ++ name ++ ")"

