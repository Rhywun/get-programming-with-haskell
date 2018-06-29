module Lesson42 where

import           Data.Array.Unboxed

aLargeList = [1 .. 10000000] :: [Int]

aLargeArray = array (0, 9999999) [] :: UArray Int Int
