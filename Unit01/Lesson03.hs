module Lesson03 where

--
-- Lambda functions
--

lf1 = (\x -> x) 4 -- 4
lf2 = (\x -> x) [1, 2, 3] -- [1,2,3]

-- QC1

qc11 = (\x -> 2 * x) 4 -- 8
qc12 = (\x -> 2 * x) 5 -- 10
qc13 = (\x -> 2 * x) 6 -- 12

--
-- Writing your own where clause
--

-- How can we rewrite this without `where`?
sumSquareOrSquareSum x y = if sumSquare > squareSum
  then sumSquare
  else squareSum
 where
  sumSquare = x ^ 2 + y ^ 2
  squareSum = (x + y) ^ 2

-- One solution - ouch:
sumSquareOrSquareSum x y =
  if (x ^ 2 + y ^ 2) > ((x + y) ^ 2) then (x ^ 2 + y ^ 2) else (x + y) ^ 2

-- Another - pass the computation functions to `body`:

body sumSquare squareSum =
  if sumSquare > squareSum then sumSquare else squareSum

sumSquareOrSquareSum' x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- Finally, replace `body` with a lambda:
sumSquareOrSquareSum'' x y
  = (\sumSquare squareSum ->
      if sumSquare > squareSum then sumSquare else squareSum
    )
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

-- QC2

{-
doubleDouble 3 -- 12
-}
doubleDouble x = dubs * 2 where dubs = x * 2

{-
doubleDouble' 3 -- 12
-}
doubleDouble' x = (\dubs -> dubs * 2) x * 2

--
-- From lambda to let
--

sumSquareOrSquareSum''' x y =
  let sumSquare = x ^ 2 + y ^ 2
      squareSum = (x + y) ^ 2
  in  if sumSquare > squareSum then sumSquare else squareSum

overwrite x = let x = 2 in let x = 3 in let x = 4 in x
  -- But... is this really overwriting?
  -- Seems to me like those x's are in different scopes, or...?

-- QC3
overwrite' x = (\x -> (\x -> (\x -> x) 4) 3) 2

--
-- Practical lambda functions and lexical scope
--

x = 4

add1 y = y + x                        -- x is bound to top-level x
                                      -- y is bound to argument y

add2 y = (\x -> y + x) 3              -- x is bound to lambda argument x
                                      -- y is bound to argument y

add3 y = (\y -> (\x -> y + x) 1) 2    -- x is bound to lambda argument x
                                      -- y is bound to lambda argument y,
                                      --   function argument is ignored

-- Q1

-- doubleDouble' x = (\dubs -> dubs * 2) x * 2
doubleDouble'' = \x -> (\dubs -> dubs * 2) x * 2

-- Q2

{-
counter1 4 -- hangs
-}
counter1 x = let x = x + 1 in let x = x + 1 in x

{-
counter2 4 = 6
-}
counter2 x = (\x -> x + 1) ((\x -> x + 1) x)
