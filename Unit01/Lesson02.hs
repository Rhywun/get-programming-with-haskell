module Lesson02 where

simple x = x

e1 = sqrt 4 -- 2.0 - Must be defined as the positive root only?

y = 10
-- y = 11 <- won't compile

calcChange owed given = if given - owed > 0 then given - owed else 0

-- Better:
calcChange' owed given = if change > 0 then change else 0
  where change = given - owed

-- QC3

doublePlusTwo x = doubleX + 2 where doubleX = x * 2

-- QC4
-- 6

--
-- Summary
--

-- Q1
-- Because an if without an else would not return a value if the if branch
-- evaluated to false.

-- Q2

inc n = n + 1
double n = 2 * n
square n = n * n

-- Q3

f n = if even n then n - 2 else 3 * n + 1
