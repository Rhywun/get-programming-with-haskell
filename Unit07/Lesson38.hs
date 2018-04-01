module Lesson38 where

import           Data.Char
import           System.IO

--
--
-- Head, partial functions, and errors
--
--
-- Dangerous! This compiles with no warning even with :set -Wall
myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

-- Now, with pattern matching, we get a warning if we don't handle []
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _      = []
myTakePM n (x:xs) = x : myTakePM (n - 1) xs
-- QC3801
myTakePM _ []     = []

-- Throwing an error - bad practice :(
myHead :: [a] -> a
myHead []    = error "empty list"
myHead (x:_) = x

--
--------------------------------------------
-- Long story short: never use head or tail!
--------------------------------------------
--
-- QC3802
-- maximum: []
-- succ: (maxBound :: Int)
-- sum: [1..]                   <-- SERIOUSLY, DO NOT TRY THIS!!!
--
--
-- Handling partial functions with Maybe
--
--
maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) =
  (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))

--
--
-- Introducing the Either type
--
--
{-
  data Either a b = Left a | Right b
-}
--
eitherHead :: [a] -> Either String a
eitherHead []     = Left "Can't take head of empty list"
eitherHead (x:xs) = Right x

--
-- QC3804
--
intExample = [1, 2, 3]

qc4 = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

--
-- Prime check
--
data PrimeError
  = TooLarge
  | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceeds limit of prime checker."
  show InvalidValue = "Value is not a valid candidate for primes."

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)  = "It's prime."
displayResult (Right False) = "It's composite."
displayResult (Left err)    = show err

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)
  where
    primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
    maxN = maximum primes

mainPrimeCheck :: IO ()
mainPrimeCheck = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a number to test if it is prime: "
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)

--
-- Q3801
--
isInt :: String -> Bool
isInt = all isDigit

-- E.g. addStrInts "123" "456"
--      addStrInts "123" "456a"
--      addStrInts "123" "456a"
--      addStrInts "123a" "456b"
addStrInts :: String -> String -> Either String Int
addStrInts x y
  | isInt x && isInt y = Right (read x + read y)
  | isInt x = Left "Number 2 is not an integer."
  | isInt y = Left "Number 1 is not an integer."
  | otherwise = Left "Both numbers are not integers."

--
-- Q3802
--
-- E.g. saferSucc (1 :: Int) == Just 2
--      saferSucc (maxBound :: Int) == Nothing
saferSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
saferSucc x
  | x == maxBound = Nothing
  | otherwise = Just (succ x)

-- E.g. saferTail [1,2,3] == [2,3]
--      saferTail [] == []
saferTail :: [a] -> [a]
saferTail []     = []
saferTail (x:xs) = xs

-- We're pretending 10 is an inifinite length
-- E.g. saferLast [1,2,3] == Right 3
--      saferLast [1,2,3,4,5,6,7,8,9,10] == Left "Can't take last of infinite list."
--      saferLast [] == Left "Can't take last of empty list."
saferLast :: [a] -> Either String a
saferLast [] = Left "Can't take last of empty list."
saferLast xs
  | length xs > 9 = Left "Can't take last of infinite list."
  | otherwise = Right (last xs)
