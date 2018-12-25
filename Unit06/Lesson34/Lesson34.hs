module Lesson34 where

--
-- Consider this
--

-- Put Book and Magazine in separate files/modules, then use import qualified to
-- import them into your main file, e.g.:
{-
import qualified Book as B
import qualified Magazine as M
-}

--

{-
head :: [a] -> a
head (x:_) = x
head [] = errorEmptyList "head"
-}

example :: [[Int]]
example = []

-- Oops - `head` is already defined in Prelude, but we can still call it like this:
{-
Lesson34.head example -- []
-}
head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty

-- QC1

length :: Int
length = 8

qc1 :: Int
qc1 = Lesson34.length * 2 -- 16

--
-- Building a multifile program with modules
-- Q1
-- see Main.hs and Palindrome.hs
--
-- Q2
-- skip --
