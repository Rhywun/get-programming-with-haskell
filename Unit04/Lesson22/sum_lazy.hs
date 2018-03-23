toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  input <- getContents
  let numbers = toInts input
  print (sum numbers)
