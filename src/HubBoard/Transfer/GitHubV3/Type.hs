{-# LANGUAGE OverloadedStrings #-}

module HubBoard.Transfer.GitHubV3.Type
    ( PostMapper(..)
    , mkPostMapper
    , module HubBoard.Transfer.Core.Type
    )
where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import           Text.Printf                    ( printf )

import           HubBoard.Transfer.Core.Type

type ToResource = Owner -> Repository -> String
type Parse output = LBS.ByteString -> output

data PostMapper input output = PostMapper ToResource input (Parse output)

mkPostMapper:: (ToJSON input, FromJSON output)=> String-> input-> (Parse output)-> PostMapper input output
mkPostMapper name = PostMapper (mkToResource name)
  where
    mkToResource :: String -> ToResource
    mkToResource name owner repository =
        printf "%s/%s/%s" owner repository name
