module Lesson38 where

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

-- cont. p. 489
