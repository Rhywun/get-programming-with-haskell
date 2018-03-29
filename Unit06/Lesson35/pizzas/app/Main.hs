module Main where

import           Lib
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "What is the size of pizza 1? "
  size1 <- getLine
  putStr "What is the cost of pizza 1? "
  cost1 <- getLine
  putStr "What is the size of pizza 2? "
  size2 <- getLine
  putStr "What is the cost of pizza 2? "
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  let betterPizza = cheaperPizza pizza1 pizza2 -- cheaper is better!
  putStrLn (describePizza betterPizza)
