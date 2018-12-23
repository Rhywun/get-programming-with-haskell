module Lesson13 where

--
-- Consider this
--

{-
inc' 1   -- 2
inc' 1.1 -- 2.1
-}
inc' :: Num a => a -> a
inc' x = x + 1

--
-- Further exploring types
--

simple :: a -> a
simple x = x

-- QC1
aList :: [String]
aList = ["cat", "dog", "mouse"]

--
-- Type classes
--

-- Example:
{-
:i Num -->
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  ...etc...
-}

-- QC2
-- Because (/) is defined in type class Fractional.

--
-- The benefits of type classes
--

-- This will work on any type that implements Num,
-- including types that haven't been written yet:
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

--
-- Defining a type class
--

class Describable a where
  describe :: a -> String

--
-- Common type classes
-- see text for discussion of Ord, Eq, Bounded, and Show
--

--
-- Deriving type classes
--

data IceCream = Chocolate | Vanilla deriving (Show, Eq, Ord)

-- QC3

-- Vanilla is greater than Chocolate because it appears last in the definition:
qc3 = Vanilla > Chocolate -- True

--
-- Summary
--

-- Q1
-- Word has the same range as Int but is composed of positive integers only.

-- Q2

inc'' :: Int -> Int
inc'' x = x + 1

-- `succ` doesn't work at bounds:
{-
succ (maxBound :: Int) -->
*** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound
-}
-- `inc` does work, but it wraps at a boundary.

-- Q3

{-
cycleSucc (maxBound :: Int)  -- -9223372036854775808
cycleSucc (maxBound :: Char) -- '\NUL'
-}
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound then minBound else succ n
