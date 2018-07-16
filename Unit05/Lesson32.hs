module Lesson32 where

import           Control.Monad
import           Data.Char

--
-- Building lists with the list monad
--

{-
powersOfTwo 10 -- [2,4,8,16,32,64,128,256,512,1024]
-}
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  n' <- [1 .. n]
  return (2 ^ n')

-- It may be easier to read with `map`:

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2 ^ x) [1 .. n]

-- But not necessarily when it starts to get more complicated:

{-
powersOfTwoAndThree 5 -- [(2,3),(4,9),(8,27),(16,81),(32,243)]
-}
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  n' <- [1 .. n]
  let powersOfTwo   = 2 ^ n'
  let powersOfThree = 3 ^ n'
  return (powersOfTwo, powersOfThree)

-- Notice with two lists we get all possible combinations:

{-
allEvenOdds 5 -- [(2,1),(2,3),(2,5),(4,1),(4,3),(4,5)]
-}
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenN <- [2, 4 .. n]
  oddN  <- [1, 3 .. n]
  return (evenN, oddN)

-- QC1

{-
pairsOfSquares 10 -- [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81),(10,100)]
-}
pairsOfSquares :: Int -> [(Int, Int)]
pairsOfSquares n = do
  n' <- [1 .. n]
  return (n', n' ^ (2 :: Int))

-- The `guard` function for filtering

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard (even value)
  return value

-- QC2

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do
  x <- xs
  guard (p x)
  return x

qc2 :: [Int]
qc2 = filter' (> 2) [1, 2, 3, 4, 5] -- [3,4,5]

--
-- List comprehensions
--

-- Before:
evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n ^ (2 :: Int)
  guard (even nSquared)
  return nSquared

-- After:
evenSquares' :: [Int]
evenSquares' =
  [ nSquared | n <- [0 .. 9], let nSquared = n ^ (2 :: Int), even nSquared ]

-- More examples:

powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [ n' ^ (2 :: Int) | n' <- [1 .. n] ]

powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n =
  [ (po2, po3) | n' <- [1 .. n], let po2 = 2 ^ n', let po3 = 3 ^ n' ]

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n = [ (e, o) | e <- [2, 4 .. n], o <- [1, 3 .. n] ]

evensGuard' :: Int -> [Int]
evensGuard' n = [ value | value <- [1 .. n], even value ]

-- QC3

qc3 :: [String]
qc3 =
  [ "Mr. " ++ uColor
  | color <- ["brown", "blue", "pink", "orange"]
  , let uColor = toUpper (head color) : tail color
  ] -- ["Mr. Brown","Mr. Blue","Mr. Pink","Mr. Orange"]

-- Q1

q1 :: [[Int]]
q1 = [ [1 .. n] | n <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] ]

-- Q2

q2 :: [[Int]]
q2 = do
  n <- [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  return [1 .. n]

q2' :: [[Int]]
q2' = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] >>= (\n -> return [1 .. n])

-- The book has this:

monthEnds :: [Int]
monthEnds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [Int] -> [Int]
dates ends = [ date | end <- ends, date <- [1 .. end] ]

datesDo :: [Int] -> [Int]
datesDo ends = do
  end  <- ends
  date <- [1 .. end]
  return date

datesMonad :: [Int] -> [Int]
-- datesMonad ends = ends >>= (\end -> [1 .. end] >>= (\date -> return date))
datesMonad ends = ends >>= (\end -> [1 .. end]) -- <-- From hlint
