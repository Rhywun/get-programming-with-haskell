module Lesson02 where

simple x = x

y = 10
-- y = 11 <- won't compile

calcChange owed given = if given - owed > 0 then given - owed else 0

-- Better:
calcChange' owed given = if change > 0 then change else 0
  where change = given - owed

-- QC0203

doublePlusTwo x = doubleX + 2 where doubleX = x * 2

-- QC0204
-- 6

-- Q0201
-- Because an if without an else would not return a value if the if branch
-- evaluated to false.

-- Q0202

inc n = n + 1
double n = 2 * n
square n = n * n

-- Q0203

f n = if even n then n - 2 else 3 * n + 1
