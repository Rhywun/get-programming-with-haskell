module Lesson22 where

--
-- QC2201
main1 :: IO ()
main1 = do
  vals <- mapM (\_ -> getLine) [1 .. 3]
  mapM_ putStrLn vals

--
-- QC2202
replicateM' :: (Monad m, Num a, Enum a) => a -> m b -> m [b]
replicateM' n f = mapM (\_ -> f) [1 .. n]

--
-- QC2203
main3 :: IO ()
main3 = do
  userInput <- getContents
  let output = reverse userInput
  putStrLn output

--
-- QC2204
--
toInts :: String -> [Int]
toInts = map read . lines

compute :: [Int] -> Int
compute ns = sum $ map (^2) ns

main4 :: IO ()
main4 = do
  input <- getContents
  let numbers = toInts input
  print (compute numbers)
