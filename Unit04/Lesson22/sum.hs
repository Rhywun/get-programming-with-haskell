import           Control.Monad
import           System.Environment

-- All the logic is wrapped up in IO - sad!

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Args: "
  mapM_ putStrLn args
  let count = if not (null args) then read (head args) else 0
  numbers <- replicateM count getLine
  let ints = map read numbers :: [Int]
  print (sum ints)
