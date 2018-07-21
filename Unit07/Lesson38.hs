module Lesson38 where

import           Data.Char
import           System.IO

--
-- Head, partial functions, and errors
--

-- Dangerous! This compiles with no warning even with :set -Wall
{-
myTake 2 [1,2,3] -- [1,2]
myTake 4 [1,2,3] -- [1,2,3,*** Exception: Prelude.head: empty list
-}
myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

-- Now, with pattern matching, we get a warning if we don't handle []
myTakePM :: Int -> [a] -> [a]
myTakePM 0 _        = []
myTakePM n (x : xs) = x : myTakePM (n - 1) xs

-- QC1
-- myTakePM _ []       = []

-- Throwing an error - bad practice, because just like above the compiler can't warn you
myHead :: [a] -> a
myHead []      = errorWithoutStackTrace "empty list"
myHead (x : _) = x

----------------------------------------------
-- Long story short: never use head (or tail)!
----------------------------------------------

-- QC2
-- maximum: []
-- succ: (maxBound :: Int)
-- sum: [1..]                   <-- SERIOUSLY, DO NOT TRY THIS!!!

--
-- Handling partial functions with Maybe
--

{-
maybeHead [1]                         -- Just 1
maybeHead []                          -- Nothing
(+ 2) <$> maybeHead [3]               -- Just 5
(+ 2) <$> maybeHead []                -- Nothing
(:) <$> maybeHead [1,2,3] <*> Just [] -- Just [1]
(:) <$> maybeHead [] <*> Just []      -- Nothing
-}
maybeHead :: [a] -> Maybe a
maybeHead []      = Nothing
maybeHead (x : _) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _         = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))

--
-- Introducing the Either type
--

{-
  data Either a b = Left a | Right b
-}

{-
eitherHead [1,2,3]          -- Right 1
eitherHead []               -- Left "Can't take head of empty list"
(+1) <$> eitherHead [3,4,5] -- Right 4
(+1) <$> eitherHead []      -- Left "Can't take head of empty list"
-}
eitherHead :: [a] -> Either String a
eitherHead []      = Left "Can't take head of empty list"
eitherHead (x : _) = Right x

-- QC4

intExample :: [Int]
intExample = [1, 2, 3]

qc4 :: Either String Int
qc4 = (+) <$> eitherHead intExample <*> eitherHead (tail intExample) -- Right 3

-- Building a prime check with Either

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceeds limit of prime checker."
  show InvalidValue = "Value is not a valid candidate for primes."

displayResult :: Either PrimeError Bool -> String
displayResult (Right True ) = "It's prime."
displayResult (Right False) = "It's composite."
displayResult (Left  err  ) = show err

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2     = Left InvalidValue
          | n > maxN  = Left TooLarge
          | otherwise = Right (n `elem` primes)
 where
  primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
  maxN   = maximum primes

mainPrimeCheck :: IO ()
mainPrimeCheck = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a number to test if it is prime: "
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)

-- Q1

isInt :: String -> Bool
isInt = all isDigit

{-
addStrInts "123" "456"   -- Right 579
addStrInts "123" "456a"  -- Left "Number 2 is not an integer."
addStrInts "123a" "456"  -- Left "Number 1 is not an integer."
addStrInts "123a" "456b" -- Left "Both numbers are not integers."
-}
addStrInts :: String -> String -> Either String Int
addStrInts x y | isInt x && isInt y = Right (read x + read y)
               | isInt x            = Left "Number 2 is not an integer."
               | isInt y            = Left "Number 1 is not an integer."
               | otherwise          = Left "Both numbers are not integers."

-- Q2

{-
saferSucc (1 :: Int)        -- Just 2
saferSucc (maxBound :: Int) -- Nothing
-}
saferSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
saferSucc x | x == maxBound = Nothing
            | otherwise     = Just (succ x)

{-
saferTail [1,2,3] -- [2,3]
saferTail []      -- []
-}
saferTail :: [a] -> [a]
saferTail []       = []
saferTail (_ : xs) = xs

-- We're pretending 10 is an inifinite length
{-
saferLast [1,2,3]                -- Right 3
saferLast [1,2,3,4,5,6,7,8,9,10] -- Left "Can't take last of infinite list."
saferLast []                     -- Left "Can't take last of empty list."
-}
saferLast :: [a] -> Either String a
saferLast [] = Left "Can't take last of empty list."
saferLast xs | length xs > 9 = Left "Can't take last of infinite list."
             | otherwise     = Right (last xs)
