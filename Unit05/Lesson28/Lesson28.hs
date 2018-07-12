module Lesson28 where

--
-- A command-line application for calculating the distance between cities
-- see dist.hs
--

--
-- Using a multi-argument function in IO using <$> and <*>
-- see min3.hs
--

--
-- Using <*> to create data in a context
--

data User = User
  { name    :: String
  , gamerId :: Int
  , score   :: Int
  } deriving (Show)

-- Note the we can create a User with regular function syntax:
-- E.g. sue == User {name = "Sue", gamerId = 1337, score = 9001}
sue = User "Sue" 1337 9001

-- Maybe context
--
serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- serverSue == Just (User {name = "Sue", gamerId = 1337, score = 9001})
serverSue = User <$> serverUsername <*> serverGamerId <*> serverScore

-- IO context
--
readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score, with ENTER after each:"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

--
-- QC2805
-- userMissingName == Nothing
userMissingName = User <$> Nothing <*> Just 2001 <*> Just 0

--
-- Q2801, 02
-- see dist.hs

-- Q2803
-- see robots.hs