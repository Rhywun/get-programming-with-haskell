import           Data.Text          (toUpper)
import qualified Data.Text.IO       as TIO
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  content <- TIO.readFile fileName
  let result = toUpper content
  TIO.writeFile fileName result
  putStrLn "Done."


-- E.g. ./capitalize hello.txt ==> should capitaliza all text