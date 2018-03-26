module Lesson29 where

{-
  class Functor f where
    fmap   :: (a -> b) -> f a -> f b              -- or <$>

  class Functor f => Applicative f where
    <*>    :: f (a -> b) -> f a -> f b
    pure   :: a -> f a
-}
--
-- QC2901
qc1 = (++) <$> Just "hello, " <*> Just "world!" -- == Just "hello, world!"

-- QC2902
qc2 = pure "Hello World!" :: IO String

-- QC2903
-- (pure +) <*> (1,2) <*> (3,4)
-- It doesn't work because (,) is not an instance of Applicative.
--
--
-- List as a context
--
--
-- ex1 == [1500,21000,2500,22000,3500,23000]   i.e. all possible sums
ex1 = pure (+) <*> [1000, 2000, 3000] <*> [500, 20000]

-- A game show example
--
doorPrize = [1000, 2000, 3000] :: [Int]

boxPrize = [500, 20000] :: [Int]

-- Deterministic:
--   Obviously, this won't compile
-- totalPrize = (+) doorPrize boxPrize
-- Non-deterministic
totalPrize = pure (+) <*> doorPrize <*> boxPrize

--
-- QC2904
-- qc4 == [10000,50000,20000,100000,30000,150000]
qc4 = pure (*) <*> doorPrize <*> [10, 50]

--
-- Prime numbers
--
-- Simple, if inefficient, prime number generator
-- E.g. primesToN 32 == [2,3,5,7,11,13,17,19,23,29,31]
primesToN :: Integer -> [Integer]
primesToN n = filter notComposite twoToN
  where
    twoToN = [2 .. n]
    composite = pure (*) <*> twoToN <*> twoToN
    notComposite = not . (`elem` composite)

--
-- Generate large amounts of test data
--
data User = User
  { name    :: String
  , gamerID :: Int
  , score   :: Int
  } deriving (Show)

testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  ]

testIDs = [1337, 0123, 999999]

testScores = [0, 100000, -99999]

testData :: [User]
testData = pure User <*> testNames <*> testIDs <*> testScores

--
-- QC2905
testNames' = "Rhywun" : testNames

testData' = pure User <*> testNames' <*> testIDs <*> testScores

qc5 = length testData' -- == 45

--
-- Q2901
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f x = pure f <*> x

--
-- Q2902
example :: Int
example = (*) ((+) 2 4) 5 -- == 30

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 5 -- == Just 30

--
-- Q2903
bought = [6, 12]

drank = [-4]

peeps = [3, 5]

perPeep = [3, 4]

-- subtract (peeps * perPeep) from bought + drank, answer is the max num of beers
q3 =
  pure (-) <*> (pure (+) <*> bought <*> drank) <*>
  (pure (*) <*> peeps <*> perPeep)

-- q3 == [-7,-10,-13,-18,-1,-4,-7,-12]
--                   ^^^
-- Therefore, you'll need to buy 18 beers.

--
--
-- Solution:
--
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
