module Lesson03 where

-- Lambda functions

simple = \ x -> x

-- QC0301
double = \x -> 2 * x

-- Writing your own where clause

sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
                           where sumSquare = x^2 + y^2
                                 squareSum = (x + y)^2

body sumSquare squareSum = if sumSquare > squareSum then sumSquare else squareSum

sumSquareOrSquareSum' x y = body (x^2 + y^2) ((x + y)^2)

sumSquareOrSquareSum'' x y = (\sumSquare squareSum ->
                              if sumSquare > squareSum
                              then sumSquare
                              else squareSum) (x^2 + y^2) ((x + y)^2)

-- QC0302
doubleDouble x = dubs * 2 where dubs = x * 2
doubleDouble' x = (\dubs -> dubs * 2) x * 2

-- From lambda to let

sumSquareOrSquareSum''' x y = let sumSquare = x^2 + y^2
                                  squareSum = (x + y)^2
                              in if sumSquare > squareSum
                                 then sumSquare
                                 else squareSum

overwrite x = let x = 2 in
              let x = 3 in
              let x = 4 in x

-- QC0303
overwrite' x = (\x -> (\x -> (\x -> x) 4) 3) 2

-- Practical lambda functions and lexical scope

x = 4

add1 y = y + x                        -- x is bound to top-level x
                                      -- y is bound to argument y

add2 y = (\x -> y + x) 3              -- x is bound to lambda argument x
                                      -- y is bound to argument y

add3 y = (\y -> (\x -> y + x) 1) 2    -- x is bound to lambda argument x
                                      -- y is bound to lambda argument y,
                                      --   function argument is ignored

-- Q0302

counter x = let x = x + 1
            in
              let x = x + 1
              in
                x

counter' x = (\x -> x + 1)
              ((\x -> x + 1) x)
