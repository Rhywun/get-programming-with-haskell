import           Data.Aeson
import           Data.ByteString.Lazy       as B
import           Data.ByteString.Lazy.Char8 as BC
import           Data.Text                  as T
import           GHC.Generics

-- Suppose we are given this JSON
sampleError = "{\"message\":\"oops!\",\"error\": 123}" :: BC.ByteString

-- We have to create a data type to match this JSON
--
-- We can't automatically derive from ToJSON or FromJSON
-- because `error` is already defined in Haskell
data ErrorMessage = ErrorMessage
  { message :: T.Text
  , error   :: Int
  } deriving (Show)

-- We have to derive the instance manually
instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

-- Just (ErrorMessage {message = "oops!", error = 123})
sampleErrorMessage = decode sampleError :: Maybe ErrorMessage

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

-- encode anErrorMessage == "{\"error\":0,\"message\":\"Everything is OK\"}"
anErrorMessage = ErrorMessage "Everything is OK" 0
