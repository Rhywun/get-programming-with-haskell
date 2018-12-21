module Lesson06 where

import           Data.List

--
-- Consider this
--

teams = ["red", "yellow", "orange", "blue", "purple"]

-- Use `cycle` and `zip` to assign these to a list of employees -
-- see `assignToGroups` below

--
-- Lists and lazy evaluation
--

simple x = x
longList = [1 ..]
stillLongList = simple longList

-- QC1
-- backwardsInfinity = reverse [1..]
-- Compiles; but don't evaluate! SERIOUSLY, DON'T EVALUATE!!

--
-- Common functions on lists
--

ix1 = "puppies" !! 4 -- 'i'

{-
ix2 "dog" -- 'g'
-}
ix2 = (!! 2) -- a section

cf1 = length [1 .. 20] -- 20

{-
isPalindrome "madam" -- True
-}
isPalindrome word = word == reverse word

{-
respond "hello"  -- "uh.. okay"
respond "hello!" -- "wow!"
-}
respond phrase = if '!' `elem` phrase then "wow!" else "uh.. okay"

{-
takeLast 10 [1..100] -- [91,92,93,94,95,96,97,98,99,100]
-}
takeLast n xs = reverse (take n (reverse xs))

{-
ones 5 -- [1,1,1,1,1]
-}
ones n = take n (cycle [1])

assignToGroups n = zip groups where groups = cycle [1 .. n]
  -- This works because `zip` stops `cycle` when one list becomes empty

threeGroups = assignToGroups
  3
  [ "file1.txt"
  , "file2.txt"
  , "file3.txt"
  , "file4.txt"
  , "file5.txt"
  , "file6.txt"
  , "file7.txt"
  , "file8.txt"
  ] -- [(1,"file1.txt"),(2,"file2.txt"),(3,"file3.txt"),
    --  (1,"file4.txt"),(2,"file5.txt"),(3,"file6.txt"), etc.]

--
-- Summary
--

-- Q1

{-
take 7 (repeat' 5) -- [5,5,5,5,5,5,5]
-}
repeat' x = cycle [x]

-- Q2

{-
subseq 2 4 "Mississippi" -- "ss"
-}
subseq from to xs = take (to - from) (drop from xs)

-- Q3

{-
inFirstHalf 'e' "hello" -- True
inFirstHalf 'o' "hello" -- False
-}
inFirstHalf x xs = x `elem` xs'
 where
  xs' = take n xs
  n   = length xs `div` 2
