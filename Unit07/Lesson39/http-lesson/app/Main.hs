module Main where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

myToken = "WkWRfDFnAuVytwSTBPTohnvHkcfXuAHx" :: BC.ByteString

noaaHost = "www.ncdc.noaa.gov" :: BC.ByteString

apiPath = "/cdo-web/api/v2/datasets" :: BC.ByteString

buildRequest ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure True $ setRequestPort 443 defaultRequest

buildRequestNoSSL ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSSL token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [token] $
  setRequestPath path $
  setRequestSecure False $ setRequestPort 80 defaultRequest

-- request = buildRequest myToken noaaHost "GET" apiPath :: Request
request = buildRequestNoSSL myToken noaaHost "GET" apiPath :: Request

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if statusCode status == 200
    then do
      putStrLn "Saving request to file..."
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print $ statusMessage status
