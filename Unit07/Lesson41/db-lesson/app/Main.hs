module Main where

import           Control.Applicative
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

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

-- Datetimes

now :: IO UTCTime
now = getCurrentTime

today :: IO Day
today = utctDay <$> now

-- User table

data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [show $ userId user, ".) ", userName user]

-- Create action

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUser :: String -> IO ()
addUser userName = withConn db $ \conn -> do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only userName)
  putStrLn "User added."

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn db $ \conn -> execute
  conn
  "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
  (userId, toolId)

-- Read action

-- We need to create instances of FromRow

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

printUsers :: IO ()
printUsers = withConn db $ \conn -> do
  response <- query_ conn "SELECT * FROM users;" :: IO [User]
  mapM_ print response

printToolQuery :: Query -> IO ()
printToolQuery q = withConn db $ \conn -> do
  response <- query_ conn q :: IO [Tool]
  mapM_ print response

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat
  ["select * from tools ", "where id not in ", "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $ mconcat
  ["select * from tools ", "where id in ", "(select tool_id from checkedout);"]

-- Update action

firstOrNothing :: [a] -> Maybe a
firstOrNothing []      = Nothing
firstOrNothing (x : _) = Just x

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  response <- query conn "SELECT * FROM tools WHERE id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing response

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool { lastReturned = date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing     = print "id not found"
updateOrWarn (Just tool) = withConn db $ \conn -> do
  let q = mconcat
        [ "UPDATE TOOLS SET  "
        , "lastReturned = ?,"
        , " timesBorrowed = ? "
        , "WHERE ID = ?;"
        ]
  execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
  print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn db $ \conn -> do
  tool       <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool <*> pure currentDay
  updateOrWarn updatedTool
