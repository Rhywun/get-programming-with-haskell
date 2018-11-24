module Main where

import           Lib
import           Data.Text                     as T
import           Data.Text.IO                  as TIO
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  TIO.putStr "Text? "
  text <- TIO.getLine
  let response = if isPalindrome text then "Palindrome!" else "No palindrome :("
  TIO.putStrLn response
