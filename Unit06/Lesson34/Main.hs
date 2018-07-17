{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO                  as TIO
                                                ( getLine
                                                , putStr
                                                , putStrLn
                                                )
import           Palindrome

main :: IO ()
main = do
  TIO.putStr "Word? "
  text <- TIO.getLine
  let response = if isPalindrome text then "It's a palindrome!" else "Not a palidrome :("
  TIO.putStrLn response
