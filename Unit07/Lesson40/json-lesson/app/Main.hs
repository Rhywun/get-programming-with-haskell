module Main where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           GHC.Generics

data NOAAResult = NOAAResult
  { uid          :: T.Text
  , mindate      :: T.Text
  , maxdate      :: T.Text
  , name         :: T.Text
  , datacoverage :: Double -- was Int - bug in the book
  , resultId     :: T.Text -- "id"
  } deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v
      .:  "uid"
      <*> v
      .:  "mindate"
      <*> v
      .:  "maxdate"
      <*> v
      .:  "name"
      <*> v
      .:  "datacoverage"
      <*> v
      .:  "id"

data Resultset = Resultset
  { offset :: Int
  , count  :: Int
  , limit  :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset

newtype Metadata = Metadata
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results  :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse

--

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = print "Error loading data."
-- Code in the book was broken
printResults (Just results) = forM_ results (print . name)

--

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults  = results <$> noaaResponse
  printResults noaaResults

--
-- Summary
--

-- Q1

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) = object
    [ "uid" .= uid
    , "mindate" .= mindate
    , "maxdate" .= maxdate
    , "name" .= name
    , "datacoverage" .= datacoverage
    , "id" .= resultId
    ]

instance ToJSON Resultset

instance ToJSON Metadata

instance ToJSON NOAAResponse

{-
> jsonData <- B.readFile "data.json"
> let noaaResponse = decode jsonData :: Maybe NOAAResponse
> encode noaaResponse
"{\"results\":[{\"uid\":\"gov.noaa.ncdc:C00861\",\"datacoverage\":1, ..."
-}

-- Q2

data IntList = EmptyList | Cons Int (IntList) deriving (Show, Generic)

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList

instance ToJSON IntList

{-
BC.putStrLn $ encode intListExample
{"tag":"Cons","contents":[1,{"tag":"Cons","contents":[2,{"tag":"EmptyList"}]}]}
-}
