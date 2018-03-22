import           Control.Monad
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let count =
        if not (null args)
          then read (head args)
          else 0
  numbers <- replicateM count getLine
  let ints = map read numbers :: [Int]
  print (sum ints)
