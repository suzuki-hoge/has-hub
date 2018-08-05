{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.Data where


import Data.Aeson (FromJSON(..), Value(Object), (.:))


newtype Label = Label String deriving (Eq)
instance FromJSON Label where
  parseJSON (Object v) = Label <$> (v .: "name")
instance Show Label where
  show (Label name) = "Label(" ++ name ++ ")"
