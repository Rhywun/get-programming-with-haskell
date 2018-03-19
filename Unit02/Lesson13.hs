module Lesson13 where

--
--
-- Further exploring types
--
--
-- :t simple --> simple :: p -> p
simple x = x

-- QC1301
-- Ans. aList :: [[Char]]
aList = ["cat", "dog", "mouse"]

--
--
-- Type classes
--
--
-- QC1302
-- Because it's defined in type class Fractional.
--
--
-- The benefits of type classes
--
--
-- This will work on any type that implements Num:
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

--
--
-- Defining a type class
--
--
class Describable a where
  describe :: a -> String

--
--
-- Deriving type classes
--
--
data Icecream
  = Chocolate
  | Vanilla
  deriving (Show, Eq, Ord)

--
-- Q1301
-- Word has the same range as Int but is composed of positive integers only.
--
-- Q1302
inc :: Int -> Int
inc x = x + 1
-- `succ` doesn't work at bounds
-- E.g. succ (maxBound :: Int) -->
--    *** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound
-- `inc` does work, but it wraps at a boundary.
--
-- Q1303
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n
