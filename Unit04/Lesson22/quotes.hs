quotes = ["Quote 1", "Quote 2", "Quote 3", "Quote 4", "Quote 5"]

-- E.g. lookupQuote ["2","1","3","n"] == ["Quote 2","Quote 1","Quote 3"]
lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : lookupQuote xs
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  input <- getContents
  mapM_ putStrLn (lookupQuote (lines input))
