module Lesson32 where

import           Control.Monad
import           Data.Char

--
--
-- Building lists with the list monad
--
--
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  n' <- [1 .. n]
  return (2 ^ n')

-- It may be easier to read with `map`:
powersOfTwoMap n = map (\x -> 2 ^ x) [1 .. n]

-- But not when it starts to get more complicated:
--
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  n' <- [1 .. n]
  let powersOfTwo = 2 ^ n'
  let powersOfThree = 3 ^ n'
  return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenN <- [2,4 .. n]
  oddN <- [1,3 .. n]
  return (evenN, oddN)

--
-- QC3201
pairsOfSquares :: Int -> [(Int, Int)]
pairsOfSquares n = do
  n' <- [1 .. n]
  let squareN = n' ^ 2
  return (n', squareN)

-- The `guard` function for filtering
--
evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

--
-- QC3202
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do
  x <- xs
  guard (p x)
  return x

qc2 = filter' (> 2) [1, 2, 3, 4, 5] -- == [3,4,5]

--
--
-- List comprehensions
--
--
-- Before:
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ 2
  guard (even nSquared)
  return nSquared

-- After:
evenSquares' = [nSquared | n <- [0 .. 9], let nSquared = n ^ 2, even nSquared]

--
{-
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  n' <- [1 .. n]
  return (2 ^ n')
-}
powersOfTwo' n = [n' ^ 2 | n' <- [1 .. n]]

{-
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  n' <- [1 .. n]
  let powersOfTwo = 2 ^ n'
  let powersOfThree = 3 ^ n'
  return (powersOfTwo, powersOfThree)
-}
powersOfTwoAndThree' n =
  [(po2, po3) | n' <- [1 .. n], let po2 = 2 ^ n', let po3 = 3 ^ n']

{-
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenN <- [2,4 .. n]
  oddN <- [1,3 .. n]
  return (evenN, oddN)
-}
allEvenOdds' n = [(e, o) | e <- [2,4 .. n], o <- [1,3 .. n]]

{-
evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value
-}
evensGuard' n = [value | value <- [1 .. n], even value]

--
-- QC3203
qc3 =
  [ "Mr. " ++ uColor
  | color <- ["brown", "blue", "pink", "orange"]
  , let uColor = toUpper (head color) : tail color
  ]

--
--
-- Q3201
q1 = [[1 .. n] | n <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]]

-- Q3202
q2 = do
  n <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  return [1 .. n]

q2' =
  [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] >>= (\n -> return [1 .. n])

--
-- The book has this:
--
monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [date | end <- ends, date <- [1 .. end]]

datesDo :: [Int] -> [Int]
datesDo ends = do
  end <- ends
  date <- [1 .. end]
  return date

datesMonad :: [Int] -> [Int]
-- datesMonad ends = ends >>= (\end -> [1 .. end] >>= (\date -> return date))
datesMonad ends = ends >>= (\end -> [1 .. end]) -- <-- From hlint
