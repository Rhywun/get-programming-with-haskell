module Lesson42 where

import           Data.Array.Unboxed
import           Data.Array.ST
import           Data.STRef
import           Control.Monad
import           Control.Monad.ST

--
-- Creating efficient arrays in Haskell with the UArray type
--

aLargeList :: [Int]
aLargeList = [1 .. 10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0, 9999999) []

{-
-- First time:
length aLargeListDoubled -- 10000000
(1.09 secs, 1,680,117,320 bytes)

-- Second time, same session:
length aLargeListDoubled -- 10000000
(0.08 secs, 116,640 bytes)
-}
aLargeListDoubled :: [Int]
aLargeListDoubled = map (* 2) aLargeList

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 4) [(3, True)]
  -- array (0,4) [(0,False),(1,False),(2,False),(3,True),(4,False)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1, 5) $ zip [1 .. 5] $ repeat True
  -- array (1,5) [(1,True),(2,True),(3,True),(4,True),(5,True)]

-- QC1
qc1 :: UArray Int Bool
qc1 = array (0, 4) [(1, True), (2, True)]
  -- array (0,4) [(0,False),(1,True),(2,True),(3,False),(4,False)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) [] -- array (0,3) [(0,0),(1,0),(2,0),(3,0)]

-- QC2
qc2 :: UArray Int Int
qc2 = array (0, 3) $ zip [0 .. 3] $ repeat 0 -- array (0,3) [(0,0),(1,0),(2,0),(3,0)]

beansInBuckets' :: UArray Int Int
beansInBuckets' = beansInBuckets // [(1, 5), (3, 6)] -- array (0,3) [(0,0),(1,5),(2,0),(3,6)]

-- Add two beans to every bucket
beansInBuckets'' :: UArray Int Int
beansInBuckets'' = accum (+) beansInBuckets' $ zip [0 .. 3] $ repeat 2
  -- array (0,3) [(0,2),(1,7),(2,2),(3,8)]

-- QC3
qc3 :: UArray Int Int
qc3 = accum (*) beansInBuckets'' $ zip [0 .. 3] $ repeat 2
  -- array (0,3) [(0,4),(1,14),(2,4),(3,16)]

--
-- Mutating state with STUArray
--

-- Transform a list of Ints into an STUArray
listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

--
-- Taking values out of the ST context
--

{-
listToUArray [1,2,3] -- array (0,2) [(0,1),(1,2),(2,3)]
-}
listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- Or, more typically, you would combine the two functions:

{-
listToUArray' [1,2,3] -- array (0,2) [(0,1),(1,2),(2,3)]
-}
listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

-- The ST type

{-
swapST (1,2) -- (2,1)
-}
swapST :: (Int, Int) -> (Int, Int)
swapST (x, y) = runST $ do
  x' <- newSTRef x
  y' <- newSTRef y
  writeSTRef x' y
  writeSTRef y' x
  xfinal <- readSTRef x'
  yfinal <- readSTRef y'
  return (xfinal, yfinal)

--
-- Implementing a bubble sort
--

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

-- QC4
myData' :: UArray Int Int
myData' = listToUArray' [7, 6, 4, 8, 10, 2]

{-
bubbleSort myData -- array (0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
                                     ^     ^     ^     ^     ^     ^
-}
bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = snd . bounds $ myArray
  forM_ [1 .. end] $ \i -> forM_ [0 .. (end - i)] $ \j -> do
    val     <- readArray stArray j
    nextVal <- readArray stArray $ j + 1
    when (val > nextVal) $ do
      writeArray stArray j       nextVal
      writeArray stArray (j + 1) val
  return stArray
