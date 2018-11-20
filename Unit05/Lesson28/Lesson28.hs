module Lesson28 where

--
-- A command-line application for calculating the distance between cities
-- see dist.hs
--

-- Using a multi-argument function in IO using <$> and <*>
-- see min3.hs

--
-- Using <*> to create data in a context
--

data User = User
  { name    :: String
  , gamerId :: Int
  , score   :: Int
  } deriving (Show)

-- Note the we can create a User with regular function syntax:
{-
sue -- User {name = "Sue", gamerId = 1337, score = 9001}
-}
sue :: User
sue = User "Sue" 1337 9001

-- Maybe context
--
maybeUsername :: Maybe String
maybeUsername = Just "Sue"

maybeGamerId :: Maybe Int
maybeGamerId = Just 1337

maybeScore :: Maybe Int
maybeScore = Just 9001

{-
maybeSue -- Just (User {name = "Sue", gamerId = 1337, score = 9001})
-}
maybeSue :: Maybe User
maybeSue = User <$> maybeUsername <*> maybeGamerId <*> maybeScore

-- IO context
--
readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score, with ENTER after each:"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

-- QC5

userMissingName :: Maybe User
userMissingName = User <$> Nothing <*> Just 2001 <*> Just 0 -- Nothing

--
-- Summary
--

-- Q1, 02
-- see dist.hs

-- Q3
-- see robots.hs
