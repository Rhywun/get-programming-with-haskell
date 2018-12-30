module Book where

import           Data.Aeson
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           GHC.Generics

-- A data type we created
-- Easy to derive from FromJSON and ToJSON because we control the field names

data Book = Book
  { title  :: T.Text
  , author :: T.Text
  , year   :: Int
  } deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook = Book {title = "Will Kurt", author = "Learn Haskell", year = 2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook
  -- "{\"year\":2017,\"author\":\"Learn Haskell\",\"title\":\"Will Kurt\"}"

e1 :: Maybe Book
e1 = decode myBookJSON
  -- Just (Book {title = "Will Kurt", author = "Learn Haskell", year = 2017})

rawJSON :: BC.ByteString
rawJSON =
  "{\"year\":1949,\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON
  -- Just Book {title = "A Short History of Decay", author = "Emil Ciroan", year = 1949})

wrongJSON :: BC.ByteString
wrongJSON =
  "{\"year\":1949,\"writer\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\"}"

bookFromWrongJSON = decode wrongJSON :: Maybe Book
  -- Nothing

bookFromWrongJSON' = eitherDecode wrongJSON :: Either String Book
  -- Left "Error in $: key \"author\" not present"

-- QC2

data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)

instance FromJSON Name
instance ToJSON Name