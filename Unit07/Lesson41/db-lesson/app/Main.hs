module Main where

import           Control.Applicative
import           Data.Time
import           Data.Time.Calendar             ( dayOfWeek ) -- requires time-1.9.2
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

--
-- Using SQLite and setting up your database
--

main = undefined :: IO ()

db = "tools.db" :: String

-- Tool table

data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool = mconcat
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

-- An aside: datetimes

now :: IO UTCTime
now = getCurrentTime -- 2019-01-01 16:54:52.03222 UTC

today :: IO Day
today = utctDay <$> now -- 2019-01-01

weekday :: IO DayOfWeek
weekday = dayOfWeek <$> today -- Tuesday

-- User table

data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

--
-- Creating data — inserting users and checking out tools
--

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> IO ()
addUser userName = withConn db $ \conn -> do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName) -- or [userName]
  putStrLn "User added."

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn db $ \conn -> execute
  conn
  "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
  (userId, toolId)

--
-- Reading data from the database and FromRow
--

-- We need to create instances of FromRow

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

--

printUsers :: IO ()
printUsers = withConn db $ \conn -> do
  response <- query_ conn "SELECT * FROM users" :: IO [User]
  mapM_ print response

--

printToolQuery :: Query -> IO ()
printToolQuery q = withConn db $ \conn -> do
  response <- query_ conn q :: IO [Tool]
  mapM_ print response

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools"

printAvailableTools :: IO ()
printAvailableTools =
  printToolQuery "SELECT * FROM tools WHERE id NOT IN (SELECT tool_id FROM checkedout)"

printCheckedoutTools :: IO ()
printCheckedoutTools =
  printToolQuery "SELECT * FROM tools WHERE id IN (SELECT tool_id FROM checkedout)"

--
-- Updating existing data
--

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  response <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing response
 where
  firstOrNothing []      = Nothing
  firstOrNothing (x : _) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool { lastReturned = date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing     = print "id not found"
updateOrWarn (Just tool) = withConn db $ \conn -> do
  let q = "UPDATE TOOLS SET lastReturned = ?, timesBorrowed = ? WHERE id = ?"
  execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
  print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn db $ \conn -> do
  tool       <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool <*> pure currentDay
  updateOrWarn updatedTool

-- snip --
