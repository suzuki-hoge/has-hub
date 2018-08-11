{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Label.Type where


import Data.Aeson (FromJSON(..), Value(Object), (.:))


newtype Label2 = Label2 String deriving (Eq, Show)
instance FromJSON Label2 where
  parseJSON (Object v) = Label2 <$> (v .: "name")
