module Lesson20 where

-- import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Semigroup                 ( Semigroup
                                                , (<>)
                                                )

--
-- Your data and the TS data type
--

file1 :: [(Int, Double)]
file1 =
  [ (1 , 200.1)
  , (2 , 199.5)
  , (3 , 199.4)
  , (4 , 198.9)
  , (5 , 199.0)
  , (6 , 200.2)
  , (9 , 200.3)
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

data TS a = TS [Int] [Maybe a] -- deriving (Show)

{-
createTS [1,2] [200.1,199.5] -- TS [1,2] [Just 200.1,Just 199.5]
                             -- (before we derived Show manually)
-}
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
 where
  completeTimes  = [minimum times .. maximum times]
  timeValueMap   = Map.fromList (zip times values)
  extendedValues = map (`Map.lookup` timeValueMap) completeTimes

{-
fileToTS file1 -- TS [1,2,3,4,5,6,...] [Just 200.1,Just 199.5,...]
               -- (before we derived Show manually)
-}
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values where (times, values) = unzip tvPairs

-- We can improve the display by deriving Show manually:

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values
      -- Moved this into the where clause just to prove I can:
      showTVPair :: Show a => Int -> Maybe a -> String
      showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
      showTVPair time Nothing      = mconcat [show time, "|NA\n"]

{-
Now:
fileToTS file1 ==
"1|200.1
2|199.5
3|199.4
..."
-}

series1 :: TS Double
series1 = fileToTS file1

series2 :: TS Double
series2 = fileToTS file2

series3 :: TS Double
series3 = fileToTS file3

series4 :: TS Double
series4 = fileToTS file4

--
-- Stitching together TS data with Semigroup and Monoid
--

-- A helper function for inserting (k, Maybe v) pairs into a Map k v
-- (This simply removes the Maybe context from the value before inserting)
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_  , Nothing   ) = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

-- Combine two TS's by creating a continuous timeline of keys, then
-- taking the values from the first TS, followed by taking
-- the values from the second TS and overwriting any matching keys
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2        = ts2
combineTS ts1        (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
 where
  bothTimes      = mconcat [t1, t2]
  completeTimes  = [minimum bothTimes .. maximum bothTimes]
  tvMap          = foldl insertMaybePair Map.empty (zip t1 v1)
  updatedMap     = foldl insertMaybePair tvMap (zip t2 v2)
  combinedValues = map (`Map.lookup` updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)

-- Now we can do this:
tsAll :: TS Double
tsAll = mconcat [series1, series2, series3, series4]

--
-- Performing calculations on your time series
--

{-
mean [1, 2, 2.1, 3] -- 2.025
-}
mean :: (Real a) => [a] -> Double
mean xs = total / count
 where
  total = (realToFrac . sum) xs
  count = (realToFrac . length) xs

{-
meanTS tsAll -- Just 210.5966666666667
-}
meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []    ) = Nothing
meanTS (TS _ values) = if all (== Nothing) values then Nothing else Just avg
 where
  justVals  = filter isJust values
  cleanVals = map fromJust justVals
  avg       = mean cleanVals

-- Min and Max

type CompareFunc a = a -> a -> a

type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

-- Construct a function that wraps other functions like `min` and `max` and
-- lets you use them with TS's
{-
makeTSCompare max (3, Just 200) (4, Just 10) -- (3, Just 200)
-}
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = f'
 where
  f' (i1, Nothing) (_, Nothing) = (i1, Nothing)
  f' (_ , Nothing) (i, val    ) = (i, val)
  f' (i , val    ) (_, Nothing) = (i, val)
  f' (i1, Just val1) (i2, Just val2) =
    if f val1 val2 == val1 then (i1, Just val1) else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS []    []    ) = Nothing
compareTS f (TS times values) = if all (== Nothing) values then Nothing else Just best
 where
  pairs = zip times values
  best  = foldl (makeTSCompare f) (0, Nothing) pairs

{-
minTS tsAll -- Just (4, Just 198.9)
-}
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

{-
maxTS tsAll -- Just (28, Just 223.8)
-}
maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

--
-- Transforming time series
--

-- diff

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing  _        = Nothing
diffPair _        Nothing  = Nothing
diffPair (Just x) (Just y) = Just (x - y)

{-
meanTS $ diffTS tsAll -- Just 0.6076923076923071
  -- this is the average increase per time period
-}
diffTS :: Num a => TS a -> TS a
diffTS (TS []    []    ) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
 where
  shiftValues = tail values
  diffValues  = zipWith diffPair shiftValues values

-- moving average: a method of smoothing

-- Calculate the mean of a list of Maybe a
{-
meanMaybe [Just 1, Just 2, Just 3]  -- Just 2.0
meanMaybe [Just 1, Just 2, Nothing] -- Nothing
-}
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if Nothing `elem` vals then Nothing else Just avg
  where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg []   _ = []
movingAvg vals n = if length nextVals == n
  then meanMaybe nextVals : movingAvg restVals n
  else []
 where
  nextVals = take n vals
  restVals = tail vals

{-
movingAverageTS tsAll 4
-}
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS []    []    ) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
 where
  ma             = movingAvg values n
  nothings       = replicate (n `div` 2) Nothing
  smoothedValues = mconcat [nothings, ma, nothings]
