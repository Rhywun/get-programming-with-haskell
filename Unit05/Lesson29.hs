module Lesson29 where

{-
  class Functor f where
    fmap   :: (a -> b) -> f a -> f b              -- or <$>

  class Functor f => Applicative f where
    <*>    :: f (a -> b) -> f a -> f b
    pure   :: a -> f a
-}

-- QC1

qc1 :: Maybe String
qc1 = (++) <$> Just "hello, " <*> Just "world!" -- == Just "hello, world!"

-- QC2

qc2 :: IO String
qc2 = pure "Hello World!"

-- QC3

-- (pure +) <*> (1,2) <*> (3,4)
-- It doesn't work because (,) is not an instance of Applicative.

--
-- List as a context
--

ex1 :: [Int]
ex1 = pure (+) <*> [1000, 2000, 3000] <*> [500, 20000]
  -- [1500,21000,2500,22000,3500,23000]   i.e. all possible sums

-- A game show example

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

-- Deterministic - obviously, this won't compile:
-- totalPrize = (+) doorPrize boxPrize

-- Non-deterministic:
totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- QC4

qc4 :: [Int]
qc4 = pure (*) <*> doorPrize <*> [10, 50] -- [10000,50000,20000,100000,30000,150000]

-- Generating the first N prime numbers

-- Composites are easy to generate with Applicative:
someComposites :: [Int]
someComposites = (*) <$> [2 .. 4] <*> [2 .. 4] -- [4,6,8,6,9,12,8,12,16]

-- Simple, if inefficient, prime number generator
{-
primesToN 32   -- [2,3,5,7,11,13,17,19,23,29,31]
primesToN 1000 -- (slow!)
-}
primesToN :: Integer -> [Integer]
primesToN n = filter notComposite twoToN
 where
  twoToN       = [2 .. n]
  composite    = (*) <$> twoToN <*> twoToN
  notComposite = not . (`elem` composite)

-- Quickly generating large amounts of test data

data User = User
  { name    :: String
  , gamerID :: Int
  , score   :: Int
  } deriving (Show)

testNames :: [String]
testNames =
  ["John Smith", "Robert'); DROP TABLE Students;--", "Christina NULL", "Randall Munroe"]

testIDs :: [Int]
testIDs = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

{-
length testData -- 36
-}
testData :: [User]
testData = pure User <*> testNames <*> testIDs <*> testScores

-- QC5

testNames'  :: [String]
testNames' = "Rhywun" : testNames

{-
length testData' -- 45
-}
testData' :: [User]
testData' = pure User <*> testNames' <*> testIDs <*> testScores

-- Q1

{-
allFmap (+ 1) [1,2,3]  -- [2,3,4]
allFmap (+ 1) (Just 5) -- Just 6
allFmap (+ 1) Nothing  -- Nothing
-}
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f x = pure f <*> x

-- Q2

example :: Int
example = (*) ((+) 2 4) 5 -- 30

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 5 -- Just 30

-- Q3

bought :: [Int]
bought = [6, 12]

drank :: [Int]
drank = [-4]

peeps :: [Int]
peeps = [3, 5]

perPeep :: [Int]
perPeep = [3, 4]

-- subtract (peeps * perPeep) from bought + drank, answer is the max num of beers
q3 :: [Int]
q3 = pure (-) <*> (pure (+) <*> bought <*> drank) <*> (pure (*) <*> peeps <*> perPeep)

-- q3 == [-7,-10,-13,-18,-1,-4,-7,-12]
--                   ^^^
-- Therefore, you'll need to buy 18 beers.

-- Solution:

startingBeer :: [Int]
startingBeer = [6, 12]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> startingBeer

guests :: [Int]
guests = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

beersPerGuest :: [Int]
beersPerGuest = [3, 4]

totalBeersNeeded :: [Int]
totalBeersNeeded = pure (*) <*> beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeersNeeded <*> remainingBeer

-- ==> 18
