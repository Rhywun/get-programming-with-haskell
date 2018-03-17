module Lesson07 where

--
-- Your first recursive function: greatest common divisor
--

gcd' a b = if remainder == 0 then b else gcd' b remainder
           where remainder = a `mod` b

sayAmount n = case n of
              1 -> "one"
              2 -> "two"
              _ -> "a bunch"

-- With pattern matching:
sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' _ = "a bunch"

empty [] = True
empty _  = False

head' (x:xs) = x
head' []     = error "No head for empty list"

-- QC0703
tail' (_:xs) = xs

-- Q0701
tail' []     = []

-- Q0702
gcd'' a 0 = a
gcd'' a b = gcd'' b (a `mod` b)
