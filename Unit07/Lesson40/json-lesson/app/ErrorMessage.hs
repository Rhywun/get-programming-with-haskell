module ErrorMessage where

import           Data.Aeson
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.Char8    as BC
import           Data.Text                     as T
import           GHC.Generics

-- Suppose we are given this JSON

sampleError = "{\"message\":\"oops!\",\"error\": 123}" :: BC.ByteString

-- We have to create a data type to match this JSON

-- We can't automatically derive from ToJSON or FromJSON
-- because `error` is already defined in Haskell

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , error   :: Int
  } deriving (Show)

-- We have to derive the instance manually

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

-- QC3

data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show)

instance FromJSON Name where
  parseJSON (Object v) = Name <$> v .: "firstName" <*> v .: "lastName"

--

sampleErrorMessage = decode sampleError :: Maybe ErrorMessage
  -- Just (ErrorMessage {message = "oops!", error = 123})

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

anErrorMessage = ErrorMessage "Everything is OK" 0
  -- encode anErrorMessage == "{\"error\":0,\"message\":\"Everything is OK\"}"

-- QC4

instance ToJSON Name where
  toJSON (Name firstName lastName) =
    object ["firstName" .= firstName, "lastName" .= lastName]

qc4 = encode (Name "Joe" "Blow") -- "{\"lastName\":\"Blow\",\"firstName\":\"Joe\"}"
