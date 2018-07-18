module Main where

import           Lib
import           Data.Text                     as T
import           Data.Text.IO                  as TIO

main :: IO ()
main = do
  TIO.putStrLn "Text?"
  text <- TIO.getLine
  let response = if isPalindrome text then "Palindrome!" else "No palindrome :("
  TIO.putStrLn response
