module Lesson21 where

import           System.Random

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main1 :: IO ()
main1 = do
  putStrLn "Hello! What's your name?"
  name <- getLine                     -- (<-) extracts the String from the IO context
  let statement = helloPerson name    -- `let` is used within `do` with non-IO types
  putStrLn statement

-- QC1
-- `getLine` retrieves the user's input. I would assume the type is String. (But
-- it's really IO String.)

-- Consider this:
-- "You can get a line of user input by using the `getLine` function. But each time
-- `getLine` is called, it can clearly return a different result."

-- It works because `getLine` runs in the IO context, which is designed for this
-- purpose.

--
-- IO types - dealing with an impure world
--

minDie = 1 :: Int

maxDie = 6 :: Int

main2 :: IO ()
main2 = do
  dieRoll <- randomRIO (minDie, maxDie)
  print dieRoll

-- QC2
-- No, because `getLine` returns IO String, not IO ().

-- QC3
-- No, because `helloPerson` takes a String, not an IO String.

--
-- An example: command-line pizza cost calculator
-- See pizza.hs
--

-- Q1

input :: Maybe String
input = Just "Joe"

maybeMain :: Maybe String
maybeMain = do
  name <- input
  let statement = helloPerson name
  return statement
