module HubBoard.Transfer.ZenHub.Type
    ( Mapper(..)
    , module HubBoard.Transfer.Core.Type
    )
where

import qualified Data.Text.Internal            as T
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types

import           HubBoard.Transfer.Core.Type

type ToResource = RepositoryId -> String
type Parse a = LBS.ByteString -> [a]

data Mapper a = Mapper ToResource (Parse a)