{-# LANGUAGE OverloadedStrings #-}


module HasHub.Object.Collaborator.IOType where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Aeson (FromJSON(..), Value(Object), (.:), decode)

import Data.Maybe (fromJust)

import HasHub.Object.Collaborator.Type

import HasHub.Connection.Type (ToResource(..))


-- input


data ReferInput = ReferInput

instance ToResource ReferInput where
  toResource _ = "/collaborators"


-- output


instance FromJSON Collaborator where
  parseJSON (Object v) = Collaborator <$> (v .: "login")

asCollaborators :: LBS.ByteString -> [Collaborator]
asCollaborators = fromJust . decode
