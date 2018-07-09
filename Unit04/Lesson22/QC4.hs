toInts :: String -> [Int]
toInts = map read . lines

compute :: [Int] -> Int
compute ns = sum $ map (^ 2) ns

main :: IO ()
main = do
  input <- getContents
  let numbers = toInts input
  print (compute numbers)
