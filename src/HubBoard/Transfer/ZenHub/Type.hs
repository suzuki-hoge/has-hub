{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.ZenHub.Type
    ( GetMapper(..)
    , UpdateMapper(..)
    , mkPostMapper
    , mkPutMapper
    , module HubBoard.Transfer.Core.Type
    )
where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types

import           Network.HTTP.Types             ( Method )
import           HubBoard.Transfer.Core.Type

type ToResource = RepositoryId -> String
type Parse output = LBS.ByteString -> [output]

data GetMapper output = GetMapper ToResource (Parse output)

data UpdateMapper input = UpdateMapper Method ToResource input

mkPostMapper :: (ToJSON input) => ToResource -> input -> UpdateMapper input
mkPostMapper = UpdateMapper "POST"

mkPutMapper :: (ToJSON input) => ToResource -> input -> UpdateMapper input
mkPutMapper = UpdateMapper "PUT"
