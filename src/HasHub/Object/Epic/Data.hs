{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Epic.Data where


import Data.Aeson (FromJSON(..), Value(Object), ToJSON(..), (.:), (.:?), (.=), object, decode)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Lazy.Internal as L (ByteString)


newtype Number = Number Int deriving (Eq)
instance Show Number where
  show (Number v) = "#" ++ show v
instance FromJSON Number where
  parseJSON (Object v) = Number <$> (v .: "issue_number")


newtype Title = Title String
instance Show Title where
  show (Title v) = v


data EpicConversionInput = EpicConversionInput
instance ToJSON EpicConversionInput where
  toJSON _ = object $ []


data Epic = Epic Number Title
instance Show Epic where
  show (Epic number title) = show number ++ " " ++ show title


parseInList :: L.ByteString -> Maybe [Number]
parseInList json = parseMaybe extract =<< decode json
  where
    extract :: Value -> Parser [Number]
    extract (Object v) = v .: "epic_issues"
    -- https://artyom.me/aeson#parsing-without-creating-extra-types
