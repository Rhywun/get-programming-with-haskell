module Lesson21 where

import           System.Random

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main1 :: IO ()
main1 = do
  putStrLn "Hello! What's your name? "
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

--
-- QC2101
-- `getLine` retrieves the user's input. I would assume the type is String.
--
--
minDie = 1 :: Int

maxDie = 6 :: Int

main2 :: IO ()
main2 = do
  dieRoll <- randomRIO (minDie, maxDie)
  print dieRoll
--
-- QC2102
-- No, because `getLine` returns IO String, not IO ().
--
--
-- QC2103
-- No, because `helloPerson` takes a String, not an IO String.
--
--
-- An example: command-line pizza cost calculator
-- See pizza.hs
--
--
-- Q2101
input = Just "Joe" :: Maybe String

maybeMain :: Maybe String
maybeMain = do
  name <- input
  let statement = helloPerson name
  return statement
