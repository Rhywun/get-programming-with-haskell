module Lesson06 where

import Data.List

--
-- Lists and lazy evaluation
--

simple x = x
longList = [1..]
stillLongList = simple longList

-- QC0601
-- backwardsInfinity = reverse [1..]
-- Compiles; but don't evaluate! SERIOUSLY, DON'T EVALUATE!!

--
-- Common functions on lists
--

isPalindrome word = word == reverse word

respond phrase = if '!' `elem` phrase then "wow!" else "uh.. okay"

takeLast n xs = reverse (take n (reverse xs))

ones n = take n (cycle [1])

assignToGroups n = zip groups
                   where groups = cycle [1..n]

e0601 = assignToGroups 3 ["file1.txt","file2.txt","file3.txt","file4.txt",
                          "file5.txt","file6.txt","file7.txt","file8.txt"]

-- Q0601
repeat' x = cycle [x]

-- Q0602
subseq from to xs = take (to - from) (drop from xs)

-- Q0603
inFirstHalf x xs = x `elem` xs'
                   where xs' = take n xs
                         n   = length xs `div` 2
