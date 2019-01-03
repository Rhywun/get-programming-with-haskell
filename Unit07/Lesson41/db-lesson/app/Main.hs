module Main where

import           Control.Applicative
import           Data.Time
import           Data.Time.Calendar             ( dayOfWeek ) -- requires time-1.9.2
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           System.IO -- for the buffering stuff

--
-- Using SQLite and setting up your database
--

-- main = undefined :: IO ()         This will be completed at the end

db = "tools.db" :: String

-- Tool table

data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Maybe Day
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

{-
> conn <- open db
> selectTool conn 1
Just 1.) hammer ...
-}
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  response <- query conn "SELECT * FROM tools WHERE id = ?" (Only toolId) :: IO [Tool]
  return $ firstOrNothing response
 where
  firstOrNothing []      = Nothing
  firstOrNothing (x : _) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool { lastReturned = Just date, timesBorrowed = 1 + timesBorrowed tool }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing     = print "id not found"
updateOrWarn (Just tool) = withConn db $ \conn -> do
  let q = "UPDATE tools SET lastReturned = ?, timesBorrowed = ? WHERE id = ?"
  execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
  print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn db $ \conn -> do
  tool       <- selectTool conn toolId
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool <*> pure currentDay
  updateOrWarn updatedTool

--
-- Deleting data from your database
--

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db"
  $ \conn -> execute conn "DELETE FROM checkedout WHERE tool_id = ?;" (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

--
-- Putting it all together
--

promptAndAddUser :: IO ()
promptAndAddUser = do
  dontBuffer
  putStr "User name? "
  userName <- getLine
  addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
  dontBuffer
  putStr "Tool name? "
  name <- getLine
  putStr "Tool description? "
  description <- getLine
  addTool name description

promptAndCheckout :: IO ()
promptAndCheckout = do
  dontBuffer
  putStr "User ID? "
  userId <- pure read <*> getLine
  putStr "Tool ID? "
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  dontBuffer
  putStr "Tool ID? "
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand command | command == "users"    = printUsers >> main
                       | command == "tools"    = printTools >> main
                       | command == "adduser"  = promptAndAddUser >> main
                       | command == "addtool"  = promptAndAddTool >> main
                       | command == "checkout" = promptAndCheckout >> main
                       | command == "checkin"  = promptAndCheckin >> main
                       | command == "in"       = printAvailableTools >> main
                       | command == "out"      = printCheckedoutTools >> main
                       | command == "quit"     = putStrLn "Bye!"
                       | otherwise = putStrLn "Error: command not found" >> main

--

dontBuffer :: IO ()
dontBuffer = hSetBuffering stdin NoBuffering

main :: IO ()
main = do
  dontBuffer
  putStr "Command? "
  command <- getLine
  performCommand command

--
-- Summary
--

-- Q1

addTool :: String -> String -> IO ()
addTool name description = withConn db $ \conn -> do
  execute conn
          "INSERT INTO tools (name, description, timesBorrowed) VALUES (?, ?, ?)"
          (name, description, 0 :: Int)
  putStrLn "Tool added."

-- `printTools` now fails because `lastReturned` is null:
-- *** Exception: ConversionFailed {errSQLType = "NULL", errHaskellType = "Day",
--   errMessage = "expecting SQLText column type"}

-- Q2
-- see `promptAndAddTool` and `performCommand` above
