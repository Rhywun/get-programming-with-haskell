module Lesson07 where

--
-- Consider this
--

{-
take' 3 [1,2,3,4] -- [1,2,3]
-}
take' 0 xs       = []
take' n (x : xs) = x : take' (n - 1) xs

--
-- Your first recursive function: greatest common divisor
--

{-
gcd' 20 16 -- 4
-}
gcd' a b = if remainder == 0 then b else gcd' b remainder where remainder = a `mod` b

-- QC2
-- No:
{-
gcd' 16 20 -- 4
-}

-- Pattern matching

-- Matching with `case`:
sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

-- With pattern matching:
sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' _ = "a bunch"

isEmpty [] = True
isEmpty _  = False

head' (x : xs) = x
head' []       = errorWithoutStackTrace "empty list"

-- QC3

{-
tail' [1,2,3] -- [2,3]
-}
tail' (_ : xs) = xs

--
-- Summary
--

-- Q1

tail' []       = []

-- Q2

{-
gcd'' 20 16 -- 4
-}
gcd'' a 0 = a
gcd'' a b = gcd'' b (a `mod` b)
