-- Warning: this gets strange - and you need to compile it and run it in
-- a terminal for it to work right.

{-
toInts ['6','2','\n','2','1','\n'] -- [62,21]
-}
toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  input <- getContents
  -- mapM_ print input
  let numbers = toInts input
  print (sum numbers)
