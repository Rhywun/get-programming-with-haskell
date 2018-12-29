module Main where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Data.String
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Control.Monad.IO.Class         ( MonadIO )

-- QC2
-- NOTE: Bug in the book - `getResponseHeader` should be `getResponseHeaders`
--                                                                         ^

response :: MonadIO m => m (Response LC.ByteString)
response = httpLBS "http://news.ycombinator.com"

qc2 :: MonadIO f => f [(HeaderName, BC.ByteString)]
qc2 = getResponseHeaders <$> response

-- `getResponseHeader` requires a HeaderName parameter:

mainQC2 :: IO ()
mainQC2 = do
  response <- httpLBS "http://news.ycombinator.com"
  print $ getResponseHeader "Server" response
  -- prints ["nginx"]

--
-- Making an HTTP request
--

myToken = "WkWRfDFnAuVytwSTBPTohnvHkcfXuAHx" :: BC.ByteString

noaaHost = "www.ncdc.noaa.gov" :: BC.ByteString

apiPath = "/cdo-web/api/v2/datasets" :: BC.ByteString

buildRequest :: BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> Request
buildRequest token host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "token" [token]
    $ setRequestPath path
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest

buildRequestNoSSL :: BC.ByteString
                  -> BC.ByteString
                  -> BC.ByteString
                  -> BC.ByteString
                  -> Request
buildRequestNoSSL token host method path =
  setRequestMethod method
    $ setRequestHost host
    $ setRequestHeader "token" [token]
    $ setRequestPath path
    $ setRequestSecure False
    $ setRequestPort 80 defaultRequest

request = buildRequest myToken noaaHost "GET" apiPath :: Request

-- Q1
request' = buildRequestNoSSL myToken noaaHost "GET" apiPath :: Request

--
-- Putting it all together
--

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if statusCode status == 200
    then do
      putStrLn "Saving request to file..."
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else print $ statusMessage status -- <- Q2
