module HasHub.Connection.Logger
(
  logRequest
, logResponse
)
where


import Network.HTTP.Client as C (Request(..), RequestBody(..))

import Data.ByteString.Char8 as BS (unpack)
import Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack)
import Data.List (intersperse)
import Data.String.Utils (strip)
import Codec.Binary.UTF8.String (decodeString)

import qualified HasHub.Connection.Config.LocalConfig as LC


logRequest :: Request -> IO ()
logRequest request = logging (toString request)
  where
    toString :: Request -> String
    toString request = let
      method = BS.unpack $ C.method request
      host = BS.unpack $ C.host request
      path = BS.unpack $ C.path request
      url = "https://" ++ host ++ path

      in case method of
        "GET" -> getRequest url
        _     -> updateRequest method url request

    getRequest :: String -> String
    getRequest url = "[GET] " ++ url

    updateRequest :: String -> String -> Request -> String
    updateRequest method url request = "[" ++ method ++ "] " ++ url ++ " " ++ parameters
      where
        (RequestBodyLBS body') = requestBody request
        body = LBS.unpack body'
        parameters = if body == "" then "{}" else decodeString body


logResponse:: LBS.ByteString -> IO ()
logResponse response = logging (toString response)
  where
    toString :: LBS.ByteString -> String
    toString = flat . LBS.unpack

    flat :: String -> String
    flat "" = "{}"
    flat s = (foldl1 (++) . intersperse " " . map strip . lines . decodeString) s


logging :: String -> IO ()
logging s = do
  rid <- LC.getRequestId
  lp <- LC.getLogPath

  appendFile lp (rid ++ ": " ++ s ++ "\n")
