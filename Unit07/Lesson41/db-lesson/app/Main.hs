module Main where

import           Control.Applicative
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

main = undefined :: IO ()

--
-- Tool
--
data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool
      , ".) "
      , name tool
      , "\n description: "
      , description tool
      , "\n last returned: "
      , show $ lastReturned tool
      , "\n times borrowed: "
      , show $ timesBorrowed tool
      , "\n"
      ]

--
-- User
--
data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

-- cont. p. 530
