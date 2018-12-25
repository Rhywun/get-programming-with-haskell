{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO                  as TIO
                                                ( getLine
                                                , putStr
                                                , putStrLn
                                                )
import           System.IO

-- Unfortunately, HIE doesn't work without a cabal file, so we lose editor
-- supprt here... but it does compile fine with ghc on the command line
import qualified Palindrome

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  TIO.putStr "Word? "
  text <- TIO.getLine
  let response = if Palindrome.isPalindrome text
        then "It's a palindrome!"
        else "Not a palidrome :("
  TIO.putStrLn response
