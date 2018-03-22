module Lesson20 where

import           Data.List
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Semigroup

--
--
-- Your data and the TS data type
--
--
file1 :: [(Int, Double)]
file1 =
  [ (1, 200.1)
  , (2, 199.5)
  , (3, 199.4)
  , (4, 198.9)
  , (5, 199.0)
  , (6, 200.2)
  , (9, 200.3)
  , (10, 201.2)
  , (12, 202.9)
  ]

file2 :: [(Int, Double)]
file2 =
  [ (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (15, 204.9)
  , (16, 207.1)
  , (18, 210.5)
  , (20, 208.8)
  ]

file3 :: [(Int, Double)]
file3 =
  [ (10, 201.2)
  , (11, 201.6)
  , (12, 201.5)
  , (13, 201.5)
  , (14, 203.5)
  , (17, 210.5)
  , (24, 215.1)
  , (25, 218.7)
  ]

file4 :: [(Int, Double)]
file4 =
  [ (26, 219.8)
  , (27, 220.5)
  , (28, 223.8)
  , (29, 222.8)
  , (30, 223.8)
  , (31, 221.7)
  , (32, 222.3)
  , (33, 220.8)
  , (34, 219.4)
  , (35, 220.1)
  , (36, 220.6)
  ]

data TS a =
  TS [Int]
     [Maybe a]
  -- deriving (Show)

-- E.g. createTS [1,2] [200.1,199.5] == TS [1,2] [Just 200.1,Just 199.5]
-- (before we derive Show manually)
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValueMap = Map.fromList (zip times values)
    extendedValues = map (`Map.lookup` timeValueMap) completeTimes

-- E.g. fileToTS file1 == TS [1,2,3,4,5,6,...] [Just 200.1,Just 199.5,...]
-- (before we derive Show manually)
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where
    (times, values) = unzip tvPairs

-- We can improve the display by deriving Show manually:
--
showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing      = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values

-- Now:
-- fileToTS file1 ==
-- "1|200.1
-- 2|199.5
-- 3|199.4
-- ..."
--
--
ts1 = fileToTS file1

ts2 = fileToTS file2

ts3 = fileToTS file3

ts4 = fileToTS file4

--
--
-- Stitching together TS data with Semigroup and Monoid
--
--
-- A helper function for inserting (k, Maybe v) pairs into a Map
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing)      = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

-- Combine two TS's by creating a continuous timeline of keys, then
-- taking the values from the first TS, followed by taking
-- the values from the second TS and overwriting any matching keys
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combinedValues = map (`Map.lookup` updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

--
-- Now we can do this:
-- mconcat [ts1, ts2, ts3, ts4]
--
--
--
-- Performing calculations on your time series
--
--
mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just avg
  where
    justVals = filter isJust values
    cleanVals = map fromJust justVals
    avg = mean cleanVals

--
-- meanTS $ mconcat [ts1, ts2, ts3, ts4] == Just 210.5966666666667
--
--
-- Min and Max
--
type CompareFunc a = a -> a -> a

type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

-- Construct a function that wraps other functions like `min` and `max` and
-- lets you use them with TS's
-- E.g. makeTSCompare max (3, Just 200) (4, Just 10) == (3, Just 200)
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = f'
  where
    f' (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    f' (_, Nothing) (i, val) = (i, val)
    f' (i, val) (_, Nothing) = (i, val)
    f' (i1, Just val1) (i2, Just val2) =
      if f val1 val2 == val1
        then (i1, Just val1)
        else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS f (TS [] []) = Nothing
compareTS f (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare f) (0, Nothing) pairs

-- E.g. minTS $ mconcat [ts1, ts2, ts3, ts4] == Just (4, Just 198.9)
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

-- E.g. maxTS $ mconcat [ts1, ts2, ts3, ts4] == Just (28, Just 223.8)
maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

-- etc.
-- PASS on the rest, come back later
