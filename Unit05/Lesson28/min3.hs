module Unit06.Lesson28.Min3 where

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = minimum [x, y, z]

readInt :: IO Int
readInt = read <$> getLine

minOfThreeInts :: IO Int
minOfThreeInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "Enter three numbers, separated by ENTER."
  n <- minOfThreeInts
  putStrLn (show n ++ " is the smallest.")

-- QC4

minOfThreeMaybeInts :: Maybe Int
minOfThreeMaybeInts = minOfThree <$> Just 10 <*> Just 3 <*> Just 6 -- Just 3
