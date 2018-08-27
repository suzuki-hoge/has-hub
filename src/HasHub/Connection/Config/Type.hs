module HasHub.Connection.Config.Type where


import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import Data.Maybe (fromMaybe)


type RequestId = String


type Owner = String


type Repository = String


type Token = String


type Endpoint = String


type RepositoryId = Int


type Proxy = String


type Resource = String


data Configs = Configs Owner Repository Token Token FilePath deriving (Show)


type Cursor = Maybe String


class QueryParser a where
  toQueryPart :: a -> Owner -> Repository -> Cursor -> String


class PaginationQueryParser a where
  parseHasNext   :: a -> LBS.ByteString -> IO Bool
  parseEndCursor :: a -> LBS.ByteString -> Maybe String


mkAfter :: Cursor -> String
mkAfter mx = maybe "" (\ x -> ", after:\"" ++ x ++ "\"") mx


class ToResource a where
  toResource :: a -> Resource


data RepositoryIdInput = RepositoryIdInput

instance QueryParser RepositoryIdInput where
  toQueryPart _ owner repository _ = unlines [
      "query {"
    , "  repository(owner:\"" ++ owner ++ "\", name:\"" ++ repository ++ "\") {"
    , "    databaseId"
    , "  }"
    , "}"
    ]

