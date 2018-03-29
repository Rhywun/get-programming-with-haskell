module Main where

import           Data.Text.IO as TIO (getLine, putStr, putStrLn)
import           Palindrome
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  TIO.putStr "Word? "
  text <- TIO.getLine
  let response =
        if isPalindrome text
          then "It's a palindrome!"
          else "Not a palidrome :("
  TIO.putStrLn response
