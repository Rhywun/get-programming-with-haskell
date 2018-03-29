module Lesson34 where

{-
head :: [a] -> a
head (x:_) = x
head [] = errorEmptyList "head"
-}

example :: [[Int]]
example = []

-- Oops - `head` is already defined in Prelude, but we can still
-- call it like this:
-- E.g. Lesson34.head example == []
head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty

-- QC3401
length = 8 :: Int
qc1 = Lesson34.length * 2 -- == 16

