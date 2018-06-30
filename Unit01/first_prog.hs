import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Who is the email for? "
  recipient <- getLine
  putStr "What is the title? "
  title <- getLine
  putStr "Who is the author? "
  author <- getLine
  putStrLn (createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying \"" ++ bookTitle ++ "\".\n"

fromPart author = "Thanks,\n" ++ author

createEmail recipient bookTitle author =
  toPart recipient ++ bodyPart bookTitle ++ fromPart author
