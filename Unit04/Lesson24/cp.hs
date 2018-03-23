import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let src = head args
  let dest = last args
  content <- readFile src
  writeFile dest content
  putStrLn "Done."
