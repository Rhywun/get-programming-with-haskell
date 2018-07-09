main :: IO ()
main = do
  userInput <- getContents
  let output = reverse userInput
  putStrLn output
